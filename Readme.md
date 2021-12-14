# Market-Sim

Dante Basile and Jocelyn Hsu

OCaml-based stock market simulator

Project for EN.601.429/629 Functional Programming in Software Engineering

## Run instructions

Run webserver-based GUI:

1. `cd` to `src` directory

2. `dune exe --root . ./market_sim.exe`

## Extra Installs

None beyond the opam deps

* ounit2 (> 2.0)) ; for each opam dependency list the version

* (core (> 0.14.1)) ; `opam list core` will display which version of core you have

* (asynch (> 0.14.0))

* (dream (> 4.3.1))

* (owl (> 1.0.1))

* (owl-plplot (> 1.0.1))

* (ppx_deriving (> 5.2.1))

* (ppx_deriving_yojson (> 3.6.1))
