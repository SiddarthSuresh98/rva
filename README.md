# rva

This is an assembler for a custom ISA nicknamed "RISC V[ECTOR]". It takes in an assembly program syntactically similar to MIPS (see input) and outputs a list of binary numbers corresponding to the instructions. The output is compatible with the [RISC V[ECTOR]](https://github.com/bdunahu/RISC-V-ECTOR-) simulator.

## Dependencies

A common-lisp implementation (SBCL) and the following libraries are required to compile:

- SBCL (tested with v2.5.2)
- ASDF (tested with v3.3.7)
- fiveam (tested with v3.3.7)
- clingon (tested with v0.5.0-1.f2a730f)
- esrap (tested with v0.18-4.d806138)

## To run

Run `make` to produce a binary file in `/bin/`. To run the unit tests, run `make test`. See the make file for further options.

## Todo

- add named register aliases
- add 'mov'', 'leave' pseudo-instructions
- allow .text and .data to appear anywhere, any number of times
- rewrite tests to be independent of each other
- write negative test cases

# About

Created at the University of Massachusetts, Amherst

CS535 -- Computer Architecture and ISA Design