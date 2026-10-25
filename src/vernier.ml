(* Output edge vernier. Documented in vernier.mli. *)

open! Base
open Hardcaml
open Signal

let ( <== ) = Signal.assign

type flop =
  { cell : string
  ; clk : string
  ; d : string
  ; reset_n : string
  ; q : string
  }

module Line_mode = struct
  type t =
    | Full
    | Coarse_only
    | Fine_only
    | Direct
  [@@deriving sexp_of, equal, enumerate]

  let to_int = function
    | Full -> 0
    | Coarse_only -> 1
    | Fine_only -> 2
    | Direct -> 3
  ;;

  let uses_coarse = function
    | Full | Coarse_only -> true
    | Fine_only | Direct -> false
  ;;

  let uses_fine = function
    | Full | Fine_only -> true
    | Coarse_only | Direct -> false
  ;;
end

let scale_frac_bits = 4
let position_frac_bits = 6
let offset_bits = 8
let skew_bits = 7
let ratio_bits = 4
let settle_bits = 3
let model_extra_bits = 4

module type Config = sig
  val pins : int
  val coarse_taps : int
  val fine_taps : int
  val fraction_bits : int
end

type cells =
  { library : Cell_library.t
  ; coarse_stage : string list
  ; fine_stage : string list
  ; inverter : string
  ; mux2 : string
  ; xor2 : string
  ; flop : flop
  }

type model =
  { base_ticks : int
  ; coarse_ticks : int
  ; fine_ticks : int
  }

type implementation =
  | Cells of cells
  | Model of model

let raise_config msg = raise_s [%message "Vernier: bad configuration" (msg : string)]

(* An equal-depth tree of 2:1 muxes selecting [inputs.(sel)]: level [l]
   pairs the nodes of level [l - 1] under select bit [l - 1]; an unpaired
   last node feeds both inputs of its mux, so every path has the same number
   of muxes and select values past the end alias to the last input. *)
let mux_tree (c : cells) ~name ~sel inputs =
  let s_pin, d0_pin, d1_pin, out_pin = Cell_pins.mux2 c.library c.mux2 in
  let rec level l nodes =
    match nodes with
    | [ x ] -> x
    | _ ->
      let rec pair j = function
        | [] -> []
        | a :: rest ->
          let b, rest =
            match rest with
            | b :: rest -> b, rest
            | [] -> a, []
          in
          Cell.instantiate
            c.library
            ~cell:c.mux2
            ~instance:(Printf.sprintf "%s%d_%d" name l j)
            [ s_pin, sel.:(l - 1); d0_pin, a; d1_pin, b ]
            out_pin
          :: pair (j + 1) rest
      in
      level (l + 1) (pair 0 nodes)
  in
  if List.length inputs < 2 then raise_config "a tap mux needs at least 2 inputs";
  if 1 lsl Signal.width sel < List.length inputs then raise_config "tap select too narrow";
  level 1 inputs
;;

module type S = sig
  include Config

  val lines : int
  val coarse_bits : int
  val fine_bits : int
  val scale_bits : int
  val ratio_bits : int
  val model_ticks_bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; level : 'a list
      ; fraction : 'a list
      ; bypass : 'a list
      ; line_mode : 'a list
      ; settle_cycles : 'a
      ; scale : 'a list
      ; offset : 'a list
      ; ratio : 'a
      ; skew : 'a
      ; raw : 'a
      ; raw_coarse : 'a
      ; raw_fine : 'a
      ; clear_flags : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { out : 'a list
      ; loopback : 'a list
      ; active_bypass : 'a list
      ; active_mode : 'a list
      ; rate_violation : 'a list
      ; saturated : 'a list
      ; mismatch : 'a list
      ; model_toggle : 'a list
      ; model_ticks : 'a list
      }
    [@@deriving hardcaml]
  end

  module Line_select : sig
    type 'a t =
      { launch : 'a
      ; coarse : 'a
      ; fine : 'a
      ; coarse_bypass : 'a
      ; fine_bypass : 'a
      }
    [@@deriving hardcaml]
  end

  module Map : sig
    module I : sig
      type 'a t =
        { fraction : 'a
        ; scale : 'a
        ; offset : 'a
        ; skew : 'a
        ; apply_skew : 'a
        ; ratio : 'a
        ; mode : 'a
        ; raw : 'a
        ; raw_coarse : 'a
        ; raw_fine : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { coarse : 'a
        ; fine : 'a
        ; saturated : 'a
        }
      [@@deriving hardcaml]
    end

    val create : Signal.t I.t -> Signal.t O.t
  end

  module Analog : sig
    type pin =
      { rise : Signal.t Line_select.t
      ; fall : Signal.t Line_select.t
      ; level : Signal.t
      ; bypass : Signal.t
      }

    type pin_out =
      { out : Signal.t
      ; loopback : Signal.t
      ; model_toggle : Signal.t * Signal.t
      ; model_ticks : Signal.t * Signal.t
      }

    val create
      :  name:string
      -> implementation
      -> clock:Signal.t
      -> clear:Signal.t
      -> pin list
      -> pin_out list
  end

  val create : ?name:string -> implementation -> Scope.t -> Signal.t I.t -> Signal.t O.t

  val hierarchical
    :  ?name:string
    -> module_name:string
    -> implementation
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end

module Make (Config : Config) : S = struct
  include Config

  let () =
    if pins < 1 || pins > 4 then raise_config "pins must be 1..4";
    if coarse_taps < 2 || coarse_taps > 64 then raise_config "coarse_taps must be 2..64";
    if fine_taps < 2 || fine_taps > 32 then raise_config "fine_taps must be 2..32";
    if fraction_bits < 3 || fraction_bits > 12
    then raise_config "fraction_bits must be 3..12"
  ;;

  let lines = 2 * pins
  let coarse_bits = num_bits_to_represent (coarse_taps - 1)
  let fine_bits = num_bits_to_represent (fine_taps - 1)
  let scale_bits = coarse_bits + scale_frac_bits
  let ratio_bits = ratio_bits
  let model_ticks_bits = fraction_bits + model_extra_bits

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; level : 'a list [@length pins]
      ; fraction : 'a list [@length pins] [@bits fraction_bits]
      ; bypass : 'a list [@length pins]
      ; line_mode : 'a list [@length pins] [@bits 2]
      ; settle_cycles : 'a [@bits settle_bits]
      ; scale : 'a list [@length lines] [@bits scale_bits]
      ; offset : 'a list [@length lines] [@bits offset_bits]
      ; ratio : 'a [@bits ratio_bits]
      ; skew : 'a [@bits skew_bits]
      ; raw : 'a
      ; raw_coarse : 'a [@bits coarse_bits]
      ; raw_fine : 'a [@bits fine_bits]
      ; clear_flags : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { out : 'a list [@length pins]
      ; loopback : 'a list [@length pins]
      ; active_bypass : 'a list [@length pins]
      ; active_mode : 'a list [@length pins] [@bits 2]
      ; rate_violation : 'a list [@length pins]
      ; saturated : 'a list [@length pins]
      ; mismatch : 'a list [@length pins]
      ; model_toggle : 'a list [@length lines]
      ; model_ticks : 'a list [@length lines] [@bits model_ticks_bits]
      }
    [@@deriving hardcaml]
  end

  module Line_select = struct
    type 'a t =
      { launch : 'a
      ; coarse : 'a [@bits coarse_bits]
      ; fine : 'a [@bits fine_bits]
      ; coarse_bypass : 'a
      ; fine_bypass : 'a
      }
    [@@deriving hardcaml]
  end

  module Map = struct
    module I = struct
      type 'a t =
        { fraction : 'a [@bits fraction_bits]
        ; scale : 'a [@bits scale_bits]
        ; offset : 'a [@bits offset_bits]
        ; skew : 'a [@bits skew_bits]
        ; apply_skew : 'a
        ; ratio : 'a [@bits ratio_bits]
        ; mode : 'a [@bits 2]
        ; raw : 'a
        ; raw_coarse : 'a [@bits coarse_bits]
        ; raw_fine : 'a [@bits fine_bits]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { coarse : 'a [@bits coarse_bits]
        ; fine : 'a [@bits fine_bits]
        ; saturated : 'a
        }
      [@@deriving hardcaml]
    end

    let pf = position_frac_bits

    (* Position width: integer part up to 2^coarse_bits (scale) plus offset,
       plus a sign bit for the skew. *)
    let pw = Int.max (coarse_bits + pf) offset_bits + 2

    let create (i : Signal.t I.t) : Signal.t O.t =
      I.Of_signal.assert_widths i;
      let shift = fraction_bits + scale_frac_bits - pf in
      let prod = uresize (i.fraction *: i.scale) ~width:(fraction_bits + scale_bits + 1) in
      let p = srl (prod +:. (1 lsl (shift - 1))) ~by:shift |> uresize ~width:pw in
      let p = p +: uresize i.offset ~width:pw in
      let p = mux2 i.apply_skew (p +: sresize i.skew ~width:pw) p in
      (* negative after a skew: clamp to 0 *)
      let p = mux2 (msb p) (zero pw) p in
      let mode_is m = i.mode ==:. Line_mode.to_int m in
      let max_c = of_int_trunc ~width:coarse_bits (coarse_taps - 1) in
      let max_f = of_int_trunc ~width:fine_bits (fine_taps - 1) in
      (* fine = round(x * R) with R = 8 + ratio/2 fine steps per coarse step,
         x in 1/2^pf coarse steps: the fraction of a step in full mode, the
         whole position (below 2 steps) in fine-only mode. *)
      let xw = pf + 1 in
      let x_clipped = p >=:. 1 lsl xw in
      let x =
        mux2
          (mode_is Fine_only)
          (mux2 x_clipped (ones xw) (sel_bottom p ~width:xw))
          (uresize (sel_bottom p ~width:pf) ~width:xw)
      in
      let fw = xw + 6 in
      let x16k = uresize (sll (uresize x ~width:fw) ~by:4) ~width:fw +: uresize (x *: i.ratio) ~width:fw in
      let f = srl (x16k +:. (1 lsl pf)) ~by:(pf + 1) in
      let f_over = f >: uresize max_f ~width:fw in
      let fine = mux2 f_over max_f (sel_bottom f ~width:fine_bits) in
      let c = srl (mux2 (mode_is Coarse_only) (p +:. (1 lsl (pf - 1))) p) ~by:pf in
      let c_over = c >: uresize max_c ~width:pw in
      let coarse = mux2 c_over max_c (sel_bottom c ~width:coarse_bits) in
      (* past the end of the coarse line: the latest tap *)
      let full_fine = mux2 c_over max_f fine in
      let mode_coarse = mux i.mode [ coarse; coarse; zero coarse_bits; zero coarse_bits ] in
      let mode_fine = mux i.mode [ full_fine; zero fine_bits; fine; zero fine_bits ] in
      let mode_sat = mux i.mode [ c_over |: f_over; c_over; x_clipped |: f_over; gnd ] in
      let clamp v m = mux2 (v >: m) m v in
      { O.coarse = mux2 i.raw (clamp i.raw_coarse max_c) mode_coarse
      ; fine = mux2 i.raw (clamp i.raw_fine max_f) mode_fine
      ; saturated = ~:(i.raw) &: mode_sat
      }
    ;;
  end

  module Analog = struct
    type pin =
      { rise : Signal.t Line_select.t
      ; fall : Signal.t Line_select.t
      ; level : Signal.t
      ; bypass : Signal.t
      }

    type pin_out =
      { out : Signal.t
      ; loopback : Signal.t
      ; model_toggle : Signal.t * Signal.t
      ; model_ticks : Signal.t * Signal.t
      }

    let cell_line (c : cells) ~name ~clock ~reset_n (s : Signal.t Line_select.t) =
      let launch =
        Cell.instantiate
          c.library
          ~cell:c.flop.cell
          ~instance:(name ^ "_launch")
          [ c.flop.clk, clock; c.flop.d, s.launch; c.flop.reset_n, reset_n ]
          c.flop.q
      in
      let inv_a, inv_y = Cell_pins.inverter c.library c.inverter in
      let inv instance a = Cell.instantiate c.library ~cell:c.inverter ~instance [ inv_a, a ] inv_y in
      let s_pin, d0_pin, d1_pin, out_pin = Cell_pins.mux2 c.library c.mux2 in
      let mux instance ~s ~d0 ~d1 =
        Cell.instantiate c.library ~cell:c.mux2 ~instance [ s_pin, s; d0_pin, d0; d1_pin, d1 ] out_pin
      in
      let coarse =
        Delay_line.create_stages
          c.library
          ~name:(name ^ "_c")
          ~stage:c.coarse_stage
          ~length:(coarse_taps - 1)
          launch
      in
      let ctree =
        mux_tree c ~name:(name ^ "_cm") ~sel:s.coarse (launch :: Array.to_list coarse.taps)
      in
      (* One inversion in either coarse path: in full mode half the muxes see
         each transition direction (see the module doc). *)
      let cbyp =
        mux
          (name ^ "_cbyp")
          ~s:s.coarse_bypass
          ~d0:(inv (name ^ "_cinv") ctree)
          ~d1:(inv (name ^ "_cbinv") launch)
      in
      let fine =
        Delay_line.create_stages
          c.library
          ~name:(name ^ "_f")
          ~stage:c.fine_stage
          ~length:(fine_taps - 1)
          cbyp
      in
      let ftree = mux_tree c ~name:(name ^ "_fm") ~sel:s.fine (cbyp :: Array.to_list fine.taps) in
      mux (name ^ "_fbyp") ~s:s.fine_bypass ~d0:ftree ~d1:cbyp
    ;;

    let cell_pin (c : cells) ~name ~clock ~reset_n (p : pin) =
      let r = cell_line c ~name:(name ^ "_r") ~clock ~reset_n p.rise in
      let f = cell_line c ~name:(name ^ "_f") ~clock ~reset_n p.fall in
      let xa, xb, xy = Cell_pins.xor2 c.library c.xor2 in
      let x = Cell.instantiate c.library ~cell:c.xor2 ~instance:(name ^ "_xor") [ xa, r; xb, f ] xy in
      let level =
        Cell.instantiate
          c.library
          ~cell:c.flop.cell
          ~instance:(name ^ "_level")
          [ c.flop.clk, clock; c.flop.d, p.level; c.flop.reset_n, reset_n ]
          c.flop.q
      in
      let s_pin, d0_pin, d1_pin, out_pin = Cell_pins.mux2 c.library c.mux2 in
      let out =
        Cell.instantiate
          c.library
          ~cell:c.mux2
          ~instance:(name ^ "_out")
          [ s_pin, p.bypass; d0_pin, x; d1_pin, level ]
          out_pin
      in
      let z = zero model_ticks_bits in
      { out; loopback = x; model_toggle = gnd, gnd; model_ticks = z, z }
    ;;

    (* Behavioural line: a launch at edge [m] with delay [d] ticks toggles
       the line output in the cycle [m + d / ticks_per_cycle], at
       [d mod ticks_per_cycle] ticks after its start. *)
    let model_line (m : model) ~clock ~clear (s : Signal.t Line_select.t) =
      let spec = Reg_spec.create ~clock ~clear () in
      let tb = model_ticks_bits in
      let max_delay = m.base_ticks + ((coarse_taps - 1) * m.coarse_ticks) + ((fine_taps - 1) * m.fine_ticks) in
      let depth = (max_delay lsr tb) + 1 in
      let dw = num_bits_to_represent max_delay + 1 in
      let launch_q = reg spec s.launch in
      let launch = s.launch ^: launch_q in
      let term sel step = uresize (sel *: of_int_trunc ~width:(num_bits_to_represent step + 1) step) ~width:dw in
      let delay =
        of_int_trunc ~width:dw m.base_ticks
        +: mux2 s.coarse_bypass (zero dw) (term s.coarse m.coarse_ticks)
        +: mux2 s.fine_bypass (zero dw) (term s.fine m.fine_ticks)
      in
      let slot_cycle = srl delay ~by:tb in
      let slot_ticks = sel_bottom delay ~width:tb in
      let valid = Array.init depth ~f:(fun _ -> wire 1) in
      let ticks = Array.init depth ~f:(fun _ -> wire tb) in
      for k = 0 to depth - 1 do
        let here = launch &: (slot_cycle ==:. k) in
        let nv, nt = if k + 1 < depth then valid.(k + 1), ticks.(k + 1) else gnd, zero tb in
        valid.(k) <== reg spec (mux2 here vdd nv);
        ticks.(k) <== reg spec (mux2 here slot_ticks nt)
      done;
      let level = reg_fb spec ~width:1 ~f:(fun l -> l ^: valid.(0)) in
      (* the line output inverts, as the cell line does *)
      ~:(level ^: valid.(0)), valid.(0), ticks.(0)
    ;;

    let model_pin m ~clock ~clear (p : pin) =
      let r, rt, rk = model_line m ~clock ~clear p.rise in
      let f, ft, fk = model_line m ~clock ~clear p.fall in
      let x = r ^: f in
      let level = reg (Reg_spec.create ~clock ~clear ()) p.level in
      { out = mux2 p.bypass level x; loopback = x; model_toggle = rt, ft; model_ticks = rk, fk }
    ;;

    let create ~name implementation ~clock ~clear pins =
      List.mapi pins ~f:(fun i (p : pin) ->
        Line_select.Of_signal.assert_widths p.rise;
        Line_select.Of_signal.assert_widths p.fall;
        let name = Printf.sprintf "%s_p%d" name i in
        match implementation with
        | Cells c -> cell_pin c ~name ~clock ~reset_n:~:clear p
        | Model m -> model_pin m ~clock ~clear p)
    ;;
  end

  module Control = struct
    type line_state =
      { select : Signal.t Line_select.t
      ; violation : Signal.t
      ; quiet : Signal.t
      ; settled : Signal.t
      }

    let create ~name scope (i : Signal.t I.t) ~loopback ~pin:p =
      let ( -- ) = Scope.naming scope in
      let nm s = Printf.sprintf "%s_p%d_%s" name p s in
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let cmode = List.nth_exn i.line_mode p in
      (* stage 0 *)
      let req = List.nth_exn i.level p in
      let lvl = reg spec req -- nm "lvl" in
      let s0_edge = reg spec (req ^: lvl) -- nm "s0_edge" in
      let s0_fall = reg spec ~:req -- nm "s0_fall" in
      let s0_frac = reg spec (List.nth_exn i.fraction p) -- nm "s0_frac" in
      let active_mode = wire 2 in
      let active_bypass = wire 1 in
      (* per line *)
      let parity = Array.init 2 ~f:(fun _ -> wire 1) in
      let line k =
        let l = (2 * p) + k in
        let pend = wire 1 in
        let iw = settle_bits + 1 in
        let idle =
          reg_fb
            spec
            ~clear_to:(ones iw)
            ~width:iw
            ~f:(fun x -> mux2 pend (zero iw) (mux2 (x ==: ones iw) x (x +:. 1)))
          -- nm (Printf.sprintf "l%d_idle" k)
        in
        let s = uresize i.settle_cycles ~width:(iw + 1) in
        let since = uresize idle ~width:(iw + 1) +:. 1 in
        let quiet = ~:pend &: (since >=: s) in
        let settled = ~:pend &: (since >=: s +:. 2) in
        let mine = s0_edge &: (if k = 0 then ~:s0_fall else s0_fall) in
        let m =
          Map.create
            { Map.I.fraction = s0_frac
            ; scale = List.nth_exn i.scale l
            ; offset = List.nth_exn i.offset l
            ; skew = i.skew
            ; apply_skew = parity.(k)
            ; ratio = i.ratio
            ; mode = active_mode
            ; raw = i.raw
            ; raw_coarse = i.raw_coarse
            ; raw_fine = i.raw_fine
            }
        in
        let sel_c = wire coarse_bits in
        let sel_f = wire fine_bits in
        let same = m.coarse ==: sel_c &: (m.fine ==: sel_f) in
        let update = mine &: (quiet |: same) in
        sel_c <== (reg spec ~enable:update m.coarse -- nm (Printf.sprintf "l%d_coarse" k));
        sel_f <== (reg spec ~enable:update m.fine -- nm (Printf.sprintf "l%d_fine" k));
        pend <== (reg spec mine -- nm (Printf.sprintf "l%d_pend" k));
        parity.(k) <== (reg spec (parity.(k) ^: pend) -- nm (Printf.sprintf "l%d_parity" k));
        let mode_is v = active_mode ==:. Line_mode.to_int v in
        ( { select =
              { Line_select.launch = parity.(k) ^: pend
              ; coarse = sel_c
              ; fine = sel_f
              ; coarse_bypass = mode_is Fine_only |: mode_is Direct
              ; fine_bypass = mode_is Coarse_only |: mode_is Direct
              }
          ; violation = mine &: ~:quiet &: ~:same
          ; quiet
          ; settled
          }
        , pend
        , mine &: m.saturated )
      in
      let r, pend_r, sat_r = line 0 in
      let f, pend_f, sat_f = line 1 in
      (* Mode changes only where both lines are quiet and no request is
         being mapped. *)
      let mode_ok = r.quiet &: f.quiet &: ~:s0_edge in
      active_mode <== (reg spec ~enable:mode_ok cmode -- nm "mode");
      active_bypass
      <== (reg
             spec
             ~clear_to:vdd
             ~enable:mode_ok
             (List.nth_exn i.bypass p)
           -- nm "bypass");
      let launched = wire 1 in
      let level_d = mux2 pend_r vdd (mux2 pend_f gnd launched) in
      launched <== (reg spec level_d -- nm "launched");
      let sync = pipeline spec ~n:2 loopback -- nm "loopback_sync" in
      let sticky name x =
        reg_fb spec ~width:1 ~f:(fun q -> mux2 i.clear_flags gnd q |: x) -- nm name
      in
      let mismatch = r.settled &: f.settled &: (sync ^: launched) in
      ( r.select
      , f.select
      , level_d
      , active_bypass
      , active_mode
      , sticky "rate_violation" (r.violation |: f.violation)
      , sticky "saturated" (sat_r |: sat_f)
      , sticky "mismatch" mismatch )
    ;;
  end

  let create ?(name = "vernier") implementation scope (i : Signal.t I.t) =
    I.Of_signal.assert_widths i;
    let loopbacks = List.init pins ~f:(fun _ -> wire 1) in
    let ctl =
      List.mapi loopbacks ~f:(fun p loopback -> Control.create ~name scope i ~loopback ~pin:p)
    in
    let analog =
      Analog.create
        ~name
        implementation
        ~clock:i.clock
        ~clear:i.clear
        (List.map ctl ~f:(fun (rise, fall, level, bypass, _, _, _, _) ->
           { Analog.rise; fall; level; bypass }))
    in
    List.iter2_exn loopbacks analog ~f:(fun w a -> w <== a.loopback);
    let pick f = List.map ctl ~f in
    { O.out = List.map analog ~f:(fun a -> a.out)
    ; loopback = List.map analog ~f:(fun a -> a.loopback)
    ; active_bypass = pick (fun (_, _, _, b, _, _, _, _) -> b)
    ; active_mode = pick (fun (_, _, _, _, m, _, _, _) -> m)
    ; rate_violation = pick (fun (_, _, _, _, _, v, _, _) -> v)
    ; saturated = pick (fun (_, _, _, _, _, _, s, _) -> s)
    ; mismatch = pick (fun (_, _, _, _, _, _, _, m) -> m)
    ; model_toggle = List.concat_map analog ~f:(fun a -> [ fst a.model_toggle; snd a.model_toggle ])
    ; model_ticks = List.concat_map analog ~f:(fun a -> [ fst a.model_ticks; snd a.model_ticks ])
    }
  ;;

  let hierarchical ?name ~module_name implementation scope =
    (match implementation with
     | Model _ when not (Scope.flatten_design scope) ->
       raise_s
         [%message
           "Vernier.Model is simulation-only: use it in a scope with ~flatten_design:true"
             (module_name : string)]
     | Model _ | Cells _ -> ());
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:module_name (create ?name implementation)
  ;;
end

module Calibration = struct
  type sweep =
    { coarse : float array
    ; fine : float array
    }
  [@@deriving sexp_of]

  type line =
    { rising : sweep
    ; falling : sweep
    }
  [@@deriving sexp_of]

  type t =
    { scale : int array
    ; offset : int array
    ; ratio : int
    ; skew : int
    ; reference_ps : float
    ; coarse_step_ps : float array
    ; fine_step_ps : float array
    }
  [@@deriving sexp_of]

  (* Least-squares line through (i, ys.(i)): intercept, slope. *)
  let fit_line ys =
    let n = Float.of_int (Array.length ys) in
    if Float.(n < 2.) then raise_s [%message "Vernier.Calibration: need at least 2 points"];
    let sx = ref 0.
    and sy = ref 0.
    and sxx = ref 0.
    and sxy = ref 0. in
    Array.iteri ys ~f:(fun i y ->
      let x = Float.of_int i in
      sx := !sx +. x;
      sy := !sy +. y;
      sxx := !sxx +. (x *. x);
      sxy := !sxy +. (x *. y));
    let slope = ((n *. !sxy) -. (!sx *. !sy)) /. ((n *. !sxx) -. (!sx *. !sx)) in
    (!sy -. (slope *. !sx)) /. n, slope
  ;;

  let clamp lo hi x = Int.max lo (Int.min hi x)

  let fit ?(margin_ps = 0.) ?ratio ~coarse_taps ~period_ps ~(mode : Line_mode.t) (lines : line array) =
    let avg a b = Array.map2_exn a b ~f:(fun x y -> (x +. y) /. 2.) in
    let fine_step =
      Array.map lines ~f:(fun l ->
        if Line_mode.uses_fine mode then snd (fit_line (avg l.rising.fine l.falling.fine)) else 1.)
    in
    let ratio_of c f = c /. f in
    let coarse_step =
      Array.mapi lines ~f:(fun i l ->
        if Line_mode.uses_coarse mode
        then snd (fit_line (avg l.rising.coarse l.falling.coarse))
        else (
          (* fine-only: a virtual coarse step of R fine steps *)
          let r =
            match ratio with
            | Some k -> 8. +. (Float.of_int k /. 2.)
            | None -> raise_s [%message "Vernier.Calibration.fit: fine-only needs ~ratio"]
          in
          r *. fine_step.(i)))
    in
    (* Zero-code delay per direction: the intercept of the axis that is used. *)
    let zero (s : sweep) =
      if Line_mode.uses_coarse mode
      then fst (fit_line s.coarse)
      else if Line_mode.uses_fine mode
      then fst (fit_line s.fine)
      else s.coarse.(0)
    in
    let z_rise = Array.map lines ~f:(fun l -> zero l.rising) in
    let z_fall = Array.map lines ~f:(fun l -> zero l.falling) in
    let reference_ps =
      Array.fold (Array.append z_rise z_fall) ~init:Float.neg_infinity ~f:Float.max +. margin_ps
    in
    let units = Float.of_int (1 lsl position_frac_bits) in
    let skew =
      Array.mapi lines ~f:(fun i _ -> (z_rise.(i) -. z_fall.(i)) /. coarse_step.(i))
      |> Array.fold ~init:0. ~f:( +. )
      |> fun x -> x /. Float.of_int (Array.length lines) *. units
    in
    let half_range = 1 lsl (skew_bits - 1) in
    let ratio =
      match ratio with
      | Some k -> k
      | None ->
        let r =
          Array.map2_exn coarse_step fine_step ~f:ratio_of
          |> Array.fold ~init:0. ~f:( +. )
          |> fun x -> x /. Float.of_int (Array.length lines)
        in
        clamp 0 ((1 lsl ratio_bits) - 1) (Float.iround_nearest_exn ((r -. 8.) *. 2.))
    in
    let scale_max = (1 lsl (num_bits_to_represent (coarse_taps - 1) + scale_frac_bits)) - 1 in
    { scale =
        Array.map coarse_step ~f:(fun c ->
          clamp
            0
            scale_max
            (Float.iround_nearest_exn (period_ps /. c *. Float.of_int (1 lsl scale_frac_bits))))
    ; offset =
        Array.mapi lines ~f:(fun i _ ->
          clamp
            0
            ((1 lsl offset_bits) - 1)
            (Float.iround_nearest_exn ((reference_ps -. z_rise.(i)) /. coarse_step.(i) *. units)))
    ; ratio
    ; skew = clamp (-half_range) (half_range - 1) (Float.iround_nearest_exn skew)
    ; reference_ps
    ; coarse_step_ps = coarse_step
    ; fine_step_ps = fine_step
    }
  ;;
end
