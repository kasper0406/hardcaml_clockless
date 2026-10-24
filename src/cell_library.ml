(** A standard-cell library, read from a Liberty file. *)

open! Base

module Direction = struct
  type t =
    | Input
    | Output
    | Inout
    | Internal
  [@@deriving sexp_of, equal]

  let of_string = function
    | "input" -> Input
    | "output" -> Output
    | "inout" -> Inout
    | _ -> Internal
  ;;
end

module Pin = struct
  type t =
    { name : string
    ; direction : Direction.t
    ; function_ : Bool_expr.t option (** for outputs; may reference IQ/IQN state *)
    ; three_state : Bool_expr.t option
    ; is_clock : bool
    }
  [@@deriving sexp_of]
end

module Kind = struct
  type t =
    | Combinational
    | Latch (** has a [latch] group *)
    | Flop (** has an [ff] group *)
    | Tristate (** an output with [three_state] *)
    | Constant (** tie cells *)
    | Physical (** no logic pins: fill, decap, antenna... *)
  [@@deriving sexp_of, equal, compare]
end

module Cell = struct
  type t =
    { name : string
    ; area : float
    ; kind : Kind.t
    ; pins : Pin.t list
    ; dont_use : bool
    }
  [@@deriving sexp_of]

  let inputs t = List.filter t.pins ~f:(fun p -> Direction.equal p.direction Input)
  let outputs t = List.filter t.pins ~f:(fun p -> Direction.equal p.direction Output)
  let pin t name = List.find t.pins ~f:(fun p -> String.equal p.name name)

  (** A single-input, single-output non-inverting cell (buffer or delay). *)
  let is_buffer t =
    match inputs t, outputs t with
    | [ i ], [ { function_ = Some (Var v); three_state = None; _ } ] -> String.equal v i.name
    | _ -> false
  ;;
end

type t =
  { name : string
  ; cells : Cell.t list
  }
[@@deriving sexp_of]

let parse_expr ~cell ~pin s =
  try Bool_expr.parse s with
  | Bool_expr.Parse_error msg ->
    raise_s [%message "bad liberty function" (cell : string) (pin : string) (msg : string)]
;;

let cell_of_group (g : Liberty.group) : Cell.t =
  let name = Liberty.group_name g in
  let pins =
    List.map (Liberty.groups g "pin") ~f:(fun p ->
      let pin = Liberty.group_name p in
      { Pin.name = pin
      ; direction =
          Option.value_map (Liberty.attr p "direction") ~default:Direction.Internal ~f:Direction.of_string
      ; function_ = Option.map (Liberty.attr p "function") ~f:(parse_expr ~cell:name ~pin)
      ; three_state = Option.map (Liberty.attr p "three_state") ~f:(parse_expr ~cell:name ~pin)
      ; is_clock = Option.equal String.equal (Liberty.attr p "clock") (Some "true")
      })
  in
  let has g' = not (List.is_empty (Liberty.groups g g')) in
  let logic_pins =
    List.filter pins ~f:(fun p ->
      match p.direction with
      | Input | Output | Inout -> true
      | Internal -> false)
  in
  let kind : Kind.t =
    if has "ff" then Flop
    else if has "latch" then Latch
    else if List.is_empty logic_pins then Physical
    else if List.exists pins ~f:(fun p -> Option.is_some p.three_state) then Tristate
    else if List.for_all logic_pins ~f:(fun p -> Direction.equal p.direction Output) then Constant
    else Combinational
  in
  { name
  ; area = Option.value_map (Liberty.attr g "area") ~default:0. ~f:Float.of_string
  ; kind
  ; pins
  ; dont_use = Option.equal String.equal (Liberty.attr g "dont_use") (Some "true")
  }
;;

let of_liberty (lib : Liberty.group) =
  { name = Liberty.group_name lib; cells = List.map (Liberty.groups lib "cell") ~f:cell_of_group }
;;

let load path = of_liberty (Liberty.parse_file path)

let find t name = List.find t.cells ~f:(fun (c : Cell.t) -> String.equal c.name name)

let find_exn t name =
  match find t name with
  | Some c -> c
  | None -> raise_s [%message "cell not in library" (name : string) ~library:(t.name : string)]
;;
