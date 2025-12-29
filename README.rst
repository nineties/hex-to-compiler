hex-to-compiler
===============

Overview
--------

This repository is an extension of `planckforth <https://github.com/nineties/planckforth>`_.

It is a personal project for experimenting with compiler bootstrapping, starting
from hand-written machine code and gradually building more capable language
processing tools. The repository primarily serves as a working log and collection
of experiments rather than a polished implementation.

Notes
-----

The project proceeds in small, incremental steps. Each stage depends only on the
artifacts produced in earlier stages, deliberately avoiding the use of external
assemblers, compilers, runtimes, and code generators.

Many parts are exploratory, incomplete, or subject to change. Documentation and
structure may lag behind the actual experiments.

Bootstrapping Process
---------------------

The system is built in explicit stages, each providing the minimum functionality
required for the next stage.

Stage0
~~~~~~

Stage0 begins with a minimal Forth interpreter written by hand in machine code.
Using Forth’s self-extensibility, this interpreter is grown into a more capable
language environment, eventually producing a simple Lisp interpreter.

Starting from hand-written machine code imposes strong constraints, but these can
be addressed by leveraging core features of Forth. Lisp is chosen as the output of
Stage0 because later stages require direct manipulation of syntax trees.

Stage1
~~~~~~

TBD

Status
------

Ongoing personal work. No guarantees of completeness, correctness, or stability.

License
-------

See the LICENSE file.
