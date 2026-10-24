(** Gate-level Verilog netlists, as written by yosys ([write_verilog -noattr
    -noexpr]) and OpenROAD ([write_verilog]): one flat module of cell
    instances with named port connections, wire declarations and [assign]
    aliases. Enough to re-extract hand-instantiated cells after synthesis or
    place-and-route ({!Sdc.check}); behavioural Verilog is rejected. *)

open! Base

exception Parse_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Parse_error s)) fmt

(* --- Lexer --- *)

type token =
  | Id of string (** plain or escaped identifier, escape removed *)
  | Num of string
  | Sym of char

let tokenize s =
  let n = String.length s in
  let toks = Queue.create () in
  let is_id_start c = Char.is_alpha c || Char.equal c '_' in
  let is_id c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$' in
  let rec skip_until i pat =
    if i >= n
    then fail "unterminated %S" pat
    else if String.is_substring_at s ~pos:i ~substring:pat
    then i + String.length pat
    else skip_until (i + 1) pat
  in
  let rec go i =
    if i < n
    then (
      let c = s.[i] in
      if Char.is_whitespace c
      then go (i + 1)
      else if String.is_substring_at s ~pos:i ~substring:"//"
      then go (Option.value_map (String.index_from s i '\n') ~default:n ~f:(fun j -> j + 1))
      else if String.is_substring_at s ~pos:i ~substring:"/*"
      then go (skip_until (i + 2) "*/")
      else if String.is_substring_at s ~pos:i ~substring:"(*"
              && not (String.is_substring_at s ~pos:i ~substring:"(*)")
      then go (skip_until (i + 2) "*)")
      else if Char.equal c '\\'
      then (
        let j = ref (i + 1) in
        while !j < n && not (Char.is_whitespace s.[!j]) do
          Int.incr j
        done;
        Queue.enqueue toks (Id (String.sub s ~pos:(i + 1) ~len:(!j - i - 1)));
        go !j)
      else if is_id_start c
      then (
        let j = ref (i + 1) in
        while !j < n && is_id s.[!j] do
          Int.incr j
        done;
        Queue.enqueue toks (Id (String.sub s ~pos:i ~len:(!j - i)));
        go !j)
      else if Char.is_digit c || Char.equal c '\''
      then (
        let j = ref (i + 1) in
        while
          !j < n
          && (Char.is_alphanum s.[!j]
              || Char.equal s.[!j] '\''
              || Char.equal s.[!j] '_'
              || Char.equal s.[!j] '?')
        do
          Int.incr j
        done;
        Queue.enqueue toks (Num (String.sub s ~pos:i ~len:(!j - i)));
        go !j)
      else (
        Queue.enqueue toks (Sym c);
        go (i + 1)))
  in
  go 0;
  Queue.to_array toks
;;

(* --- Numbers: sized or unsized constants, as bits msb first --- *)

let const_bits s =
  match String.lsplit2 s ~on:'\'' with
  | None -> fail "unsized decimal constant %s in a connection" s
  | Some (size, rest) ->
    let rest = String.lstrip rest ~drop:(fun c -> Char.equal c 's' || Char.equal c 'S') in
    if String.is_empty rest then fail "bad constant %s" s;
    let base = Char.lowercase rest.[0] in
    let digits = String.filter (String.drop_prefix rest 1) ~f:(fun c -> not (Char.equal c '_')) in
    let bits_of_digit per d =
      match Char.lowercase d with
      | 'x' | 'z' | '?' -> fail "x/z constant %s in a netlist" s
      | d ->
        let v =
          if Char.is_digit d then Char.to_int d - Char.to_int '0' else Char.to_int d - Char.to_int 'a' + 10
        in
        List.init per ~f:(fun k -> (v lsr (per - 1 - k)) land 1 = 1)
    in
    let bits =
      match base with
      | 'b' -> List.concat_map (String.to_list digits) ~f:(bits_of_digit 1)
      | 'o' -> List.concat_map (String.to_list digits) ~f:(bits_of_digit 3)
      | 'h' -> List.concat_map (String.to_list digits) ~f:(bits_of_digit 4)
      | 'd' ->
        let v = Int.of_string digits in
        List.init 62 ~f:(fun k -> (v lsr (61 - k)) land 1 = 1)
      | _ -> fail "bad constant %s" s
    in
    if String.is_empty size
    then bits
    else (
      let w = Int.of_string size in
      let len = List.length bits in
      if len >= w then List.drop bits (len - w) else List.init (w - len) ~f:(fun _ -> false) @ bits)
;;

(* --- Netlist --- *)

module Conn = struct
  type t =
    | Net of int
    | Open
  [@@deriving sexp_of, equal]
end

module Instance = struct
  type t =
    { name : string
    ; cell : string
    ; pins : (string * Conn.t) list (** one entry per connected bit; multi-bit ports as [pin[i]] *)
    }
  [@@deriving sexp_of]
end

type t =
  { module_name : string
  ; net_names : string list array (** every name of each net; net 0 is constant 0, net 1 constant 1 *)
  ; instances : Instance.t list
  ; by_name : (string, Instance.t) Hashtbl.t
  ; net_of_name : (string, int) Hashtbl.t
  }

let const0 = 0
let const1 = 1
let net_names t n = t.net_names.(n)
let find_instance t name = Hashtbl.find t.by_name name
let find_net t name = Hashtbl.find t.net_of_name name
let instances t = t.instances
let num_nets t = Array.length t.net_names

(* Raw module: declarations, aliases and instances by bit name. *)
type raw_module =
  { name : string
  ; widths : (string, int list) Hashtbl.t (** bit indices, msb first; [] for scalars *)
  ; mutable assigns : (string list * string list) list (** lhs bits, rhs bits *)
  ; mutable insts : (string * string * (string * string list option) list) list
  (** cell, instance, (pin, bits msb first or None if open) *)
  }

let parse_modules text =
  let toks = tokenize text in
  let pos = ref 0 in
  let peek () = if !pos < Array.length toks then Some toks.(!pos) else None in
  let next () =
    match peek () with
    | Some t ->
      Int.incr pos;
      t
    | None -> fail "unexpected end of file"
  in
  let describe = function
    | Id s -> s
    | Num s -> s
    | Sym c -> String.of_char c
  in
  let expect_sym c =
    match next () with
    | Sym d when Char.equal c d -> ()
    | t -> fail "expected '%c', got %s" c (describe t)
  in
  let ident () =
    match next () with
    | Id s -> s
    | t -> fail "expected an identifier, got %s" (describe t)
  in
  let int_tok () =
    match next () with
    | Num s -> Int.of_string s
    | t -> fail "expected a number, got %s" (describe t)
  in
  let is_sym c = function
    | Some (Sym d) -> Char.equal c d
    | _ -> false
  in
  let range () =
    (* after '[' *)
    let hi = int_tok () in
    if is_sym ':' (peek ())
    then (
      ignore (next () : token);
      let lo = int_tok () in
      expect_sym ']';
      hi, lo)
    else (
      expect_sym ']';
      hi, hi)
  in
  let indices (hi, lo) =
    if hi >= lo then List.init (hi - lo + 1) ~f:(fun k -> hi - k) else List.init (lo - hi + 1) ~f:(fun k -> hi + k)
  in
  let skip_to_semi () =
    while not (is_sym ';' (peek ())) do
      ignore (next () : token)
    done;
    ignore (next () : token)
  in
  let modules = ref [] in
  let rec top () =
    match peek () with
    | None -> ()
    | Some (Id "module") ->
      ignore (next () : token);
      modules := parse_module () :: !modules;
      top ()
    | Some t -> fail "expected 'module', got %s" (describe t)
  and parse_module () =
    let m =
      { name = ident (); widths = Hashtbl.create (module String); assigns = []; insts = [] }
    in
    let bits_of name =
      match Hashtbl.find m.widths name with
      | Some [] | None -> [ name ]
      | Some ix -> List.map ix ~f:(fun i -> Printf.sprintf "%s[%d]" name i)
    in
    let declare names r =
      List.iter names ~f:(fun nm ->
        Hashtbl.set m.widths ~key:nm ~data:(Option.value_map r ~default:[] ~f:indices))
    in
    (* declaration after its keyword: [signed] [range] names *)
    let decl ~stop =
      let rec mods r =
        match peek () with
        | Some (Id ("wire" | "reg" | "signed" | "logic" | "tri")) ->
          ignore (next () : token);
          mods r
        | Some (Sym '[') ->
          ignore (next () : token);
          mods (Some (range ()))
        | _ -> r
      in
      let r = mods None in
      let rec names acc =
        let nm = ident () in
        match peek () with
        | Some (Sym ',') ->
          ignore (next () : token);
          (match peek () with
           | Some (Id ("input" | "output" | "inout")) -> List.rev (nm :: acc)
           | _ -> names (nm :: acc))
        | _ -> List.rev (nm :: acc)
      in
      declare (names []) r;
      ignore stop
    in
    (* Header: non-ANSI port names, or ANSI declarations. *)
    if is_sym '#' (peek ()) then fail "module parameters are not supported";
    if is_sym '(' (peek ())
    then (
      ignore (next () : token);
      let rec ports () =
        match peek () with
        | Some (Sym ')') -> ignore (next () : token)
        | Some (Sym ',') ->
          ignore (next () : token);
          ports ()
        | Some (Id ("input" | "output" | "inout")) ->
          ignore (next () : token);
          decl ~stop:')';
          ports ()
        | Some (Id _) ->
          ignore (next () : token);
          ports ()
        | Some t -> fail "unexpected %s in port list of %s" (describe t) m.name
        | None -> fail "unexpected end of file"
      in
      ports ());
    expect_sym ';';
    (* An expression in a connection or assign, as bit names msb first. *)
    let rec expr () =
      match next () with
      | Sym '{' ->
        let rec items acc =
          let e = expr () in
          match next () with
          | Sym ',' -> items (e :: acc)
          | Sym '}' -> List.concat (List.rev (e :: acc))
          | t -> fail "expected ',' or '}', got %s" (describe t)
        in
        items []
      | Num s -> List.map (const_bits s) ~f:(fun b -> if b then "1'b1" else "1'b0")
      | Id nm ->
        if is_sym '[' (peek ())
        then (
          ignore (next () : token);
          let r = range () in
          List.map (indices r) ~f:(fun i -> Printf.sprintf "%s[%d]" nm i))
        else bits_of nm
      | t -> fail "unexpected %s in an expression" (describe t)
    in
    let rec items () =
      match next () with
      | Id "endmodule" -> ()
      | Id ("input" | "output" | "inout" | "wire" | "reg" | "tri" | "supply0" | "supply1") ->
        decl ~stop:';';
        expect_sym ';';
        items ()
      | Id "assign" ->
        let rec one () =
          let lhs = expr () in
          expect_sym '=';
          let rhs = expr () in
          m.assigns <- (lhs, rhs) :: m.assigns;
          match next () with
          | Sym ',' -> one ()
          | Sym ';' -> ()
          | t -> fail "expected ',' or ';', got %s" (describe t)
        in
        one ();
        items ()
      | Id ("defparam" | "specify" | "timescale") ->
        skip_to_semi ();
        items ()
      | Id
          (( "always"
           | "always_ff"
           | "always_comb"
           | "initial"
           | "function"
           | "task"
           | "generate"
           | "parameter"
           | "localparam" ) as kw) ->
        fail "%s in module %s: not a gate-level netlist" kw m.name
      | Id cell ->
        if is_sym '#' (peek ())
        then (
          (* parameter overrides: skip the balanced parentheses *)
          ignore (next () : token);
          expect_sym '(';
          let depth = ref 1 in
          while !depth > 0 do
            match next () with
            | Sym '(' -> Int.incr depth
            | Sym ')' -> Int.decr depth
            | _ -> ()
          done);
        let inst = ident () in
        expect_sym '(';
        let rec conns acc =
          match next () with
          | Sym ')' -> List.rev acc
          | Sym ',' -> conns acc
          | Sym '.' ->
            let pin = ident () in
            expect_sym '(';
            let bits =
              if is_sym ')' (peek ())
              then None
              else Some (expr ())
            in
            expect_sym ')';
            conns ((pin, bits) :: acc)
          | t -> fail "instance %s: only named port connections are supported, got %s" inst (describe t)
        in
        let cs = conns [] in
        expect_sym ';';
        m.insts <- (cell, inst, cs) :: m.insts;
        items ()
      | t -> fail "unexpected %s in module %s" (describe t) m.name
    in
    items ();
    m.assigns <- List.rev m.assigns;
    m.insts <- List.rev m.insts;
    m
  in
  top ();
  List.rev !modules
;;

let of_string ?top text =
  let modules = parse_modules text in
  let defined = List.map modules ~f:(fun (m : raw_module) -> m.name) in
  let m =
    match top with
    | Some name ->
      (match List.find modules ~f:(fun (m : raw_module) -> String.equal m.name name) with
       | Some m -> m
       | None -> fail "no module %s" name)
    | None ->
      let used =
        List.concat_map modules ~f:(fun m -> List.map m.insts ~f:(fun (cell, _, _) -> cell))
      in
      (match List.filter modules ~f:(fun m -> not (List.mem used m.name ~equal:String.equal)) with
       | [ m ] -> m
       | ms ->
         fail
           "cannot pick the top module among %s: pass ~top"
           (String.concat ~sep:", " (List.map ms ~f:(fun m -> m.name))))
  in
  List.iter m.insts ~f:(fun (cell, inst, _) ->
    if List.mem defined cell ~equal:String.equal
    then fail "instance %s of module %s: hierarchical netlists are not supported (flatten first)" inst cell);
  (* union-find over bit names *)
  let ids = Hashtbl.create (module String) in
  let parent = ref (Array.create ~len:1024 0) in
  let count = ref 0 in
  let id name =
    Hashtbl.find_or_add ids name ~default:(fun () ->
      let i = !count in
      if i >= Array.length !parent
      then (
        let a = Array.create ~len:(2 * i) 0 in
        Array.blito ~src:!parent ~dst:a ();
        parent := a);
      !parent.(i) <- i;
      Int.incr count;
      i)
  in
  ignore (id "1'b0" : int);
  ignore (id "1'b1" : int);
  let rec find i =
    let p = !parent.(i) in
    if p = i
    then i
    else (
      let r = find p in
      !parent.(i) <- r;
      r)
  in
  let union a b =
    let a = find a
    and b = find b in
    (* keep the constants as roots, so constant nets are recognisable *)
    if a <> b then if a <= 1 then !parent.(b) <- a else !parent.(a) <- b
  in
  Hashtbl.iter_keys m.widths ~f:(fun nm ->
    match Hashtbl.find_exn m.widths nm with
    | [] -> ignore (id nm : int)
    | ix -> List.iter ix ~f:(fun i -> ignore (id (Printf.sprintf "%s[%d]" nm i) : int)));
  List.iter m.assigns ~f:(fun (lhs, rhs) ->
    let rec zip l r =
      match l, r with
      | [], _ -> ()
      | _, [] ->
        (* rhs narrower: zero-extended *)
        List.iter l ~f:(fun b -> union (id b) const0)
      | _ ->
        let ll = List.length l
        and lr = List.length r in
        if lr > ll
        then zip l (List.drop r (lr - ll))
        else if ll > lr
        then (
          List.iter (List.take l (ll - lr)) ~f:(fun b -> union (id b) const0);
          zip (List.drop l (ll - lr)) r)
        else List.iter2_exn l r ~f:(fun a b -> union (id a) (id b))
    in
    zip lhs rhs);
  let insts_raw =
    List.map m.insts ~f:(fun (cell, inst, cs) ->
      ( cell
      , inst
      , List.concat_map cs ~f:(fun (pin, bits) ->
          match bits with
          | None -> [ pin, None ]
          | Some [ b ] -> [ pin, Some (id b) ]
          | Some bs ->
            let w = List.length bs in
            List.mapi bs ~f:(fun k b -> Printf.sprintf "%s[%d]" pin (w - 1 - k), Some (id b))) ))
  in
  if Array.exists (Array.init 2 ~f:Fn.id) ~f:(fun c -> find c <> c)
  then fail "constants 0 and 1 are shorted";
  if find const0 = find const1 then fail "constants 0 and 1 are shorted";
  (* compact nets: 0 and 1 are the constants *)
  let canon = Hashtbl.create (module Int) in
  Hashtbl.set canon ~key:(find const0) ~data:const0;
  Hashtbl.set canon ~key:(find const1) ~data:const1;
  let next = ref 2 in
  let net_of_id i =
    Hashtbl.find_or_add canon (find i) ~default:(fun () ->
      let n = !next in
      Int.incr next;
      n)
  in
  let net_of_name = Hashtbl.create (module String) in
  Hashtbl.iteri ids ~f:(fun ~key ~data -> Hashtbl.set net_of_name ~key ~data:(net_of_id data));
  let net_names = Array.create ~len:!next [] in
  Hashtbl.iteri net_of_name ~f:(fun ~key ~data -> net_names.(data) <- key :: net_names.(data));
  let net_names = Array.map net_names ~f:(List.sort ~compare:String.compare) in
  let instances =
    List.map insts_raw ~f:(fun (cell, name, pins) ->
      { Instance.name
      ; cell
      ; pins =
          List.map pins ~f:(fun (p, c) ->
            p, Option.value_map c ~default:Conn.Open ~f:(fun i -> Conn.Net (net_of_id i)))
      })
  in
  let by_name = Hashtbl.create (module String) in
  List.iter instances ~f:(fun i ->
    match Hashtbl.add by_name ~key:i.name ~data:i with
    | `Ok -> ()
    | `Duplicate -> fail "duplicate instance %s" i.name);
  { module_name = m.name; net_names; instances; by_name; net_of_name }
;;

let load ?top path = of_string ?top (Stdio.In_channel.read_all path)
