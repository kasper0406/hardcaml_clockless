(lang dune 3.17)

(name hardcaml_clockless)

(generate_opam_files true)

(license MIT)

(authors kasper0406)

(source
 (github kasper0406/hardcaml_clockless))

(package
 (name hardcaml_clockless)
 (synopsis "Sub-clock and clockless circuit support for Hardcaml")
 (description
  "Standard-cell-level building blocks for Hardcaml designs that work below or without a clock: liberty-driven cell instantiation, delay lines and time-to-digital converters, with the constraints and simulation needed to verify them.")
 (depends
  (ocaml
   (>= 5.1))
  (hardcaml
   (>= v0.18~))
  (ppx_hardcaml
   (>= v0.18~))
  (ppx_jane
   (>= v0.18~))
  stdio))
