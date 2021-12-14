(lang dune 2.9)

(generate_opam_files true)
  (authors "Dante Basile" "Jocelyn Hsu")
  (maintainers "dbasile1070@gmail.com")
  (package
   (name market_sim) ; put in your own project name here
   (synopsis "An OCaml library and application for stock-market simulation")
   (description "The library provides functions for simulating many common stock market operations. The executable allows for operation of these functions by GUI")
   (depends
    (ounit2 (> 2.0)) ; for each opam dependency list the version
    (core (> 0.14.1)) ; `opam list core` will display which version of core you have
    (asynch (> 0.14.0))
    (dream (> 4.3.1))
    (owl (> 1.0.1))
    (owl-plplot (> 1.0.1))
    (ppx_deriving (> 5.2.1))
    (ppx_deriving_yojson (> 3.6.1))
 ))
