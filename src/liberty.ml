(** A small parser for the Liberty (.lib) format: just enough structure to read
    cells, pins, boolean functions and timing tables. *)

open! Base

type value =
  | Str of string (** quoted *)
  | Word of string (** unquoted identifier or number *)
[@@deriving sexp_of, equal]

type stmt =
  | Attr of string * value (** [name : value ;] *)
  | Complex of string * value list (** [name (v, ...) ;] *)
  | Group of group (** [name (v, ...) { ... }] *)

and group =
  { kind : string
  ; args : value list
  ; body : stmt list
  }
[@@deriving sexp_of]

let value_to_string = function
  | Str s | Word s -> s
;;

(* --- Lexer ------------------------------------------------------------- *)

type token =
  | T_word of string
  | T_str of string
  | T_punct of char
  | T_eof

exception Parse_error of string

let fail ~line fmt = Printf.ksprintf (fun s -> raise (Parse_error (Printf.sprintf "line %d: %s" line s))) fmt

let tokenize (s : string) : (token * int) array =
  let n = String.length s in
  let toks = Queue.create () in
  let line = ref 1 in
  let i = ref 0 in
  let is_word_char c =
    Char.is_alphanum c || (match c with '_' | '.' | '-' | '+' | '!' | '\'' | '*' | '&' | '|' | '^' | '$' | '/' | '[' | ']' -> true | _ -> false)
  in
  while !i < n do
    let c = s.[!i] in
    if Char.equal c '\n' then (Int.incr line; Int.incr i)
    else if Char.is_whitespace c then Int.incr i
    else if Char.equal c '\\' then Int.incr i (* line continuation *)
    else if Char.equal c '/' && !i + 1 < n && Char.equal s.[!i + 1] '*' then (
      i := !i + 2;
      while !i + 1 < n && not (Char.equal s.[!i] '*' && Char.equal s.[!i + 1] '/') do
        if Char.equal s.[!i] '\n' then Int.incr line;
        Int.incr i
      done;
      i := !i + 2)
    else if Char.equal c '/' && !i + 1 < n && Char.equal s.[!i + 1] '/' then
      while !i < n && not (Char.equal s.[!i] '\n') do Int.incr i done
    else if Char.equal c '"' then (
      let start = !i + 1 in
      Int.incr i;
      while !i < n && not (Char.equal s.[!i] '"') do
        if Char.equal s.[!i] '\n' then Int.incr line;
        Int.incr i
      done;
      if !i >= n then fail ~line:!line "unterminated string";
      let raw = String.sub s ~pos:start ~len:(!i - start) in
      (* drop backslash-newline continuations inside strings *)
      let str =
        String.concat_map raw ~f:(fun ch -> if Char.equal ch '\\' || Char.equal ch '\n' then "" else String.of_char ch)
      in
      Queue.enqueue toks (T_str str, !line);
      Int.incr i)
    else if List.mem [ '('; ')'; '{'; '}'; ':'; ';'; ',' ] c ~equal:Char.equal then (
      Queue.enqueue toks (T_punct c, !line);
      Int.incr i)
    else if is_word_char c then (
      let start = !i in
      while !i < n && is_word_char s.[!i] do Int.incr i done;
      Queue.enqueue toks (T_word (String.sub s ~pos:start ~len:(!i - start)), !line))
    else fail ~line:!line "unexpected character %C" c
  done;
  Queue.enqueue toks (T_eof, !line);
  Queue.to_array toks
;;

(* --- Parser ------------------------------------------------------------ *)

let parse_string (src : string) : group =
  let toks = tokenize src in
  let pos = ref 0 in
  let peek () = fst toks.(!pos) in
  let line () = snd toks.(!pos) in
  let advance () = Int.incr pos in
  let expect c =
    match peek () with
    | T_punct c' when Char.equal c c' -> advance ()
    | _ -> fail ~line:(line ()) "expected '%c'" c
  in
  let value () =
    match peek () with
    | T_word w -> advance (); Word w
    | T_str s -> advance (); Str s
    | _ -> fail ~line:(line ()) "expected a value"
  in
  let args () =
    expect '(';
    let rec loop acc =
      match peek () with
      | T_punct ')' -> advance (); List.rev acc
      | T_punct ',' -> advance (); loop acc
      | _ -> loop (value () :: acc)
    in
    loop []
  in
  let rec stmts acc =
    match peek () with
    | T_punct '}' | T_eof -> List.rev acc
    | T_word name ->
      advance ();
      (match peek () with
       | T_punct ':' ->
         advance ();
         let v = value () in
         (* tolerate unquoted multi-token values up to ';' *)
         let rec rest v =
           match peek () with
           | T_punct ';' -> advance (); v
           | T_punct '}' -> v
           | _ ->
             let v' = value () in
             rest (Word (value_to_string v ^ " " ^ value_to_string v'))
         in
         stmts (Attr (name, rest v) :: acc)
       | T_punct '(' ->
         let a = args () in
         (match peek () with
          | T_punct '{' ->
            advance ();
            let body = stmts [] in
            expect '}';
            stmts (Group { kind = name; args = a; body } :: acc)
          | T_punct ';' -> advance (); stmts (Complex (name, a) :: acc)
          | _ -> stmts (Complex (name, a) :: acc))
       | _ -> fail ~line:(line ()) "unexpected token after '%s'" name)
    | T_punct ';' -> advance (); stmts acc
    | _ -> fail ~line:(line ()) "unexpected token"
  in
  match stmts [] with
  | [ Group g ] -> g
  | _ -> raise (Parse_error "expected a single top-level library group")
;;

let parse_file path = parse_string (Stdio.In_channel.read_all path)

(* --- Accessors --------------------------------------------------------- *)

let attr (g : group) name =
  List.find_map g.body ~f:(function
    | Attr (n, v) when String.equal n name -> Some (value_to_string v)
    | _ -> None)
;;

let groups (g : group) kind =
  List.filter_map g.body ~f:(function
    | Group g' when String.equal g'.kind kind -> Some g'
    | _ -> None)
;;

let group_name (g : group) =
  match g.args with
  | v :: _ -> value_to_string v
  | [] -> ""
;;

(** The arguments of a complex attribute [name (v, ...);]. *)
let complex (g : group) name =
  List.find_map g.body ~f:(function
    | Complex (n, vs) when String.equal n name -> Some vs
    | _ -> None)
;;

(** All the numbers in a list of values, e.g. [index_1 ("0.1, 0.2")] or
    [values ("1, 2", "3, 4")] (row by row). *)
let floats (vs : value list) =
  List.concat_map vs ~f:(fun v ->
    String.split_on_chars (value_to_string v) ~on:[ ','; ' '; '\t'; '\n' ]
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:Float.of_string)
;;

(** The rows of a [values (...)] table: one row per value. *)
let float_rows (vs : value list) = List.map vs ~f:(fun v -> floats [ v ])
