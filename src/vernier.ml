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

module Mode = struct
  type t =
    | Bypass
    | Coarse
    | Fine
    | Full
  [@@deriving sexp_of, equal, enumerate]

  let to_int = function
    | Bypass -> 0
    | Coarse -> 1
    | Fine -> 2
    | Full -> 3
  ;;

  let uses_coarse = function
    | Bypass | Coarse | Full -> true
    | Fine -> false
  ;;

  let uses_fine = function
    | Bypass | Fine | Full -> true
    | Coarse -> false
  ;;
end

let scale_frac_bits = 4
let position_frac_bits = 6
let offset_bits = 8
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
  val model_ticks_bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; level : 'a list
      ; fraction : 'a list
      ; mode : 'a list
      ; settle_cycles : 'a
      ; scale : 'a list
      ; offset : 'a list
      ; ratio : 'a list
      ; clear_flags : 'a list
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { out : 'a list
      ; loopback : 'a list
      ; line_out : 'a list
      ; active_mode : 'a list
      ; fallback : 'a list
      ; overrun : 'a list
      ; range_fault : 'a list
      ; mismatch : 'a list
      ; overrun_event : 'a list
      ; range_event : 'a list
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
        ; ratio : 'a
        ; mode : 'a
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { coarse : 'a
        ; fine : 'a
        ; range_fault : 'a
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
      ; line_out : Signal.t * Signal.t
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
  let model_ticks_bits = fraction_bits + model_extra_bits

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; level : 'a list [@length pins]
      ; fraction : 'a list [@length pins] [@bits fraction_bits]
      ; mode : 'a list [@length pins] [@bits 2]
      ; settle_cycles : 'a [@bits settle_bits]
      ; scale : 'a list [@length lines] [@bits scale_bits]
      ; offset : 'a list [@length lines] [@bits offset_bits]
      ; ratio : 'a list [@length lines] [@bits ratio_bits]
      ; clear_flags : 'a list [@length pins]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { out : 'a list [@length pins]
      ; loopback : 'a list [@length pins]
      ; line_out : 'a list [@length lines]
      ; active_mode : 'a list [@length pins] [@bits 2]
      ; fallback : 'a list [@length pins]
      ; overrun : 'a list [@length pins]
      ; range_fault : 'a list [@length pins]
      ; mismatch : 'a list [@length pins]
      ; overrun_event : 'a list [@length pins]
      ; range_event : 'a list [@length pins]
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
        ; ratio : 'a [@bits ratio_bits]
        ; mode : 'a [@bits 2]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { coarse : 'a [@bits coarse_bits]
        ; fine : 'a [@bits fine_bits]
        ; range_fault : 'a
        }
      [@@deriving hardcaml]
    end

    let pf = position_frac_bits

    let create (i : Signal.t I.t) : Signal.t O.t =
      I.Of_signal.assert_widths i;
      let mode_is m = i.mode ==:. Mode.to_int m in
      let max_c = of_int_trunc ~width:coarse_bits (coarse_taps - 1) in
      let max_f = of_int_trunc ~width:fine_bits (fine_taps - 1) in
      (* position p in 1/2^pf coarse steps; its integer part fits coarse_bits
         + 1 bits (scale < 2^coarse_bits, offset < 4 steps) *)
      let pw = Int.max (coarse_bits + pf) offset_bits + 1 in
      let shift = fraction_bits + scale_frac_bits - pf in
      let prod = uresize (i.fraction *: i.scale) ~width:(fraction_bits + scale_bits + 1) in
      let p = srl (prod +:. (1 lsl (shift - 1))) ~by:shift |> uresize ~width:pw in
      let p = p +: uresize i.offset ~width:pw in
      let c = srl p ~by:pf in
      let c_over = c >: uresize max_c ~width:pw in
      (* fine = round(rho * R), R = 8 + ratio / 2 fine steps per coarse step,
         rho = the remainder in 1/2^pf steps *)
      let rho = sel_bottom p ~width:pf in
      let fw = pf + ratio_bits + 2 in
      let x = uresize (sll (uresize rho ~width:fw) ~by:4) ~width:fw +: uresize (rho *: i.ratio) ~width:fw in
      let f = srl (x +:. (1 lsl pf)) ~by:(pf + 1) in
      let f_over = f >: uresize max_f ~width:fw in
      let identity = i.scale ==:. 0 in
      let f_frac = uresize i.fraction ~width:(Int.max fraction_bits coarse_bits) in
      let clamp_c v = mux2 (v >: uresize max_c ~width:(width v)) max_c (sel_bottom v ~width:coarse_bits) in
      let low_fine = sel_bottom (uresize i.fraction ~width:(Int.max fraction_bits fine_bits)) ~width:fine_bits in
      let fine_direct = mux2 (low_fine >: max_f) max_f low_fine in
      let coarse = mux2 identity (clamp_c f_frac) (mux2 c_over max_c (sel_bottom c ~width:coarse_bits)) in
      let fine =
        mux2
          (mode_is Fine)
          fine_direct
          (mux2
             (identity |: mode_is Coarse)
             (zero fine_bits)
             (mux2 (c_over |: f_over) max_f (sel_bottom f ~width:fine_bits)))
      in
      { O.coarse = mux2 (mode_is Fine) (zero coarse_bits) coarse
      ; fine
      ; range_fault =
          ~:identity
          &: (mode_is Coarse &: c_over |: (mode_is Full &: (c_over |: f_over)))
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
      ; line_out : Signal.t * Signal.t
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
      { out; loopback = x; line_out = r, f; model_toggle = gnd, gnd; model_ticks = z, z }
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
      { out = mux2 p.bypass level x; loopback = x; line_out = r, f; model_toggle = rt, ft; model_ticks = rk, fk }
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
    type t =
      { rise : Signal.t Line_select.t
      ; fall : Signal.t Line_select.t
      ; level : Signal.t
      ; bypass : Signal.t
      ; active_mode : Signal.t
      ; fallback : Signal.t
      ; overrun : Signal.t
      ; range_fault : Signal.t
      ; mismatch : Signal.t
      ; overrun_event : Signal.t
      ; range_event : Signal.t
      }

    let create ~name scope (i : Signal.t I.t) ~loopback ~pin:p =
      let ( -- ) = Scope.naming scope in
      let nm s = Printf.sprintf "%s_p%d_%s" name p s in
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let clear_flags = List.nth_exn i.clear_flags p in
      (* stage 0: register the request *)
      let req = List.nth_exn i.level p in
      let lvl = reg spec req -- nm "lvl" in
      let s0_edge = reg spec (req ^: lvl) -- nm "s0_edge" in
      let s0_fall = reg spec ~:req -- nm "s0_fall" in
      let s0_frac = reg spec (List.nth_exn i.fraction p) -- nm "s0_frac" in
      let active_mode = wire 2 in
      let mode_is m = active_mode ==:. Mode.to_int m in
      let vernier = ~:(mode_is Bypass) in
      (* one mapper per pin: a pin has at most one edge per cycle *)
      let line_const l = List.nth_exn l (2 * p), List.nth_exn l ((2 * p) + 1) in
      let pick l = let r, f = line_const l in mux2 s0_fall f r in
      let m =
        Map.create
          { Map.I.fraction = s0_frac
          ; scale = pick i.scale
          ; offset = pick i.offset
          ; ratio = pick i.ratio
          ; mode = active_mode
          }
      in
      let line k =
        let ln s = nm (Printf.sprintf "%s_%s" (if k = 0 then "r" else "f") s) in
        let pend = wire 1 in
        let iw = settle_bits + 1 in
        let idle =
          reg_fb spec ~clear_to:(ones iw) ~width:iw ~f:(fun x ->
            mux2 pend (zero iw) (mux2 (x ==: ones iw) x (x +:. 1)))
          -- ln "idle"
        in
        let s = uresize i.settle_cycles ~width:(iw + 1) in
        let since = uresize idle ~width:(iw + 1) +:. 1 in
        let quiet = ~:pend &: (since >=: s) in
        let settled = ~:pend &: (since >=: s +:. 2) in
        let mine = s0_edge &: if k = 0 then ~:s0_fall else s0_fall in
        let sel_c = wire coarse_bits in
        let sel_f = wire fine_bits in
        let same = m.coarse ==: sel_c &: (m.fine ==: sel_f) in
        let update = mine &: (quiet |: same) in
        sel_c <== (reg spec ~enable:update m.coarse -- ln "coarse");
        sel_f <== (reg spec ~enable:update m.fine -- ln "fine");
        pend <== (reg spec mine -- ln "pend");
        let parity = reg_fb spec ~width:1 ~f:(fun q -> q ^: pend) -- ln "parity" in
        ( { Line_select.launch = parity ^: pend
          ; coarse = sel_c
          ; fine = sel_f
          ; coarse_bypass = mode_is Fine
          ; fine_bypass = mode_is Coarse
          }
        , mine &: ~:quiet &: ~:same
        , quiet
        , settled
        , pend )
      in
      let rise, over_r, quiet_r, settled_r, pend_r = line 0 in
      let fall, over_f, quiet_f, settled_f, pend_f = line 1 in
      (* Mode changes apply only where both lines are quiet and no request is
         being mapped. *)
      let mode_ok = quiet_r &: quiet_f &: ~:s0_edge in
      active_mode <== (reg spec ~enable:mode_ok (List.nth_exn i.mode p) -- nm "mode");
      let launched = wire 1 in
      let level_d = mux2 pend_r vdd (mux2 pend_f gnd launched) in
      launched <== (reg spec level_d -- nm "launched");
      let sync = pipeline spec ~n:2 loopback -- nm "loopback_sync" in
      let sticky name x =
        reg_fb spec ~width:1 ~f:(fun q -> mux2 clear_flags gnd q |: x) -- nm name
      in
      let overrun_event = reg spec (vernier &: (over_r |: over_f)) -- nm "overrun_event" in
      let range_event = reg spec (s0_edge &: m.range_fault) -- nm "range_event" in
      let overrun = sticky "overrun" overrun_event in
      let range_fault = sticky "range_fault" range_event in
      (* A fault drops the pin to bypass at once: the pin shows the requested
         level at cycle resolution. It returns once the flags are cleared and
         both lines are quiet, so the switch back moves no edge in flight. *)
      let fallback =
        reg_fb spec ~width:1 ~f:(fun q ->
          overrun_event |: range_event |: (q &: ~:(mode_ok &: ~:overrun &: ~:range_fault)))
        -- nm "fallback"
      in
      { rise
      ; fall
      ; level = level_d
      ; bypass = mode_is Bypass |: fallback
      ; active_mode
      ; fallback
      ; overrun
      ; range_fault
      ; mismatch = sticky "mismatch" (settled_r &: settled_f &: (sync ^: launched))
      ; overrun_event
      ; range_event
      }
    ;;
  end

  let create ?(name = "vernier") implementation scope (i : Signal.t I.t) =
    I.Of_signal.assert_widths i;
    let loopbacks = List.init pins ~f:(fun _ -> wire 1) in
    let ctl = List.mapi loopbacks ~f:(fun p loopback -> Control.create ~name scope i ~loopback ~pin:p) in
    let analog =
      Analog.create
        ~name
        implementation
        ~clock:i.clock
        ~clear:i.clear
        (List.map ctl ~f:(fun (c : Control.t) ->
           { Analog.rise = c.rise; fall = c.fall; level = c.level; bypass = c.bypass }))
    in
    List.iter2_exn loopbacks analog ~f:(fun w a -> w <== a.loopback);
    let pair f = List.concat_map analog ~f:(fun a -> let r, f = f a in [ r; f ]) in
    { O.out = List.map analog ~f:(fun a -> a.out)
    ; loopback = List.map analog ~f:(fun a -> a.loopback)
    ; line_out = pair (fun a -> a.line_out)
    ; active_mode = List.map ctl ~f:(fun c -> c.active_mode)
    ; fallback = List.map ctl ~f:(fun c -> c.fallback)
    ; overrun = List.map ctl ~f:(fun c -> c.overrun)
    ; range_fault = List.map ctl ~f:(fun c -> c.range_fault)
    ; mismatch = List.map ctl ~f:(fun c -> c.mismatch)
    ; overrun_event = List.map ctl ~f:(fun c -> c.overrun_event)
    ; range_event = List.map ctl ~f:(fun c -> c.range_event)
    ; model_toggle = pair (fun a -> a.model_toggle)
    ; model_ticks = pair (fun a -> a.model_ticks)
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
    ; ratio : int array
    ; d_align_ps : float
    ; coarse_step_ps : float array
    ; fine_step_ps : float array
    ; skew_ps : float array
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

  let fit ?(margin_ps = 0.) ~coarse_taps ~period_ps ~(mode : Mode.t) (lines : line array) =
    (match mode with
     | Coarse | Full -> ()
     | Bypass | Fine -> raise_s [%message "Vernier.Calibration.fit: only Coarse and Full are calibrated"]);
    let avg a b = Array.map2_exn a b ~f:(fun x y -> (x +. y) /. 2.) in
    let coarse_fit = Array.map lines ~f:(fun l -> fit_line (avg l.rising.coarse l.falling.coarse)) in
    let coarse_step = Array.map coarse_fit ~f:snd in
    let fine_step = Array.map lines ~f:(fun l -> snd (fit_line (avg l.rising.fine l.falling.fine))) in
    let zero = Array.map coarse_fit ~f:fst in
    let skew_ps =
      Array.map lines ~f:(fun l -> fst (fit_line l.rising.coarse) -. fst (fit_line l.falling.coarse))
    in
    let d_align_ps = Array.fold zero ~init:Float.neg_infinity ~f:Float.max +. margin_ps in
    let units = Float.of_int (1 lsl position_frac_bits) in
    (* coarse mode rounds to the nearest tap: half a step more *)
    let half = match mode with Coarse -> 0.5 | _ -> 0. in
    let scale_max = (1 lsl (num_bits_to_represent (coarse_taps - 1) + scale_frac_bits)) - 1 in
    { scale =
        Array.map coarse_step ~f:(fun c ->
          clamp 1 scale_max (Float.iround_nearest_exn (period_ps /. c *. Float.of_int (1 lsl scale_frac_bits))))
    ; offset =
        Array.mapi zero ~f:(fun i z ->
          clamp 0 ((1 lsl offset_bits) - 1)
            (Float.iround_nearest_exn (((d_align_ps -. z) /. coarse_step.(i) +. half) *. units)))
    ; ratio =
        Array.mapi coarse_step ~f:(fun i c ->
          clamp 0 ((1 lsl ratio_bits) - 1) (Float.iround_nearest_exn ((c /. fine_step.(i) -. 8.) *. 2.)))
    ; d_align_ps
    ; coarse_step_ps = coarse_step
    ; fine_step_ps = fine_step
    ; skew_ps
    }
  ;;
end
