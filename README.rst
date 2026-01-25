hex-to-compiler
===============

.. image:: https://github.com/nineties/hex-to-compiler/actions/workflows/runtests.yaml/badge.svg?branch=main&event=push
   :target: https://github.com/nineties/hex-to-compiler/actions/workflows/runtests.yaml
   :alt: run tests

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

Stage1: Hex -> Forth -> Lisp
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Stage1 begins with a minimal Forth interpreter written by hand in machine code.
Using Forth’s self-extensibility, this interpreter is grown into a more capable
language environment, eventually producing a simple Lisp interpreter.

Starting from hand-written machine code imposes strong constraints, but these can
be addressed by leveraging core features of Forth.

Lisp is selected as the target of Stage1 because subsequent stages involve building
tools such as assemblers. Lisp is well suited for this role, as it allows
easy construction of internal DSLs and direct manipulation of syntax trees.

Stage2: Building Tools
~~~~~~~~~~~~~~~~~~~~~~

In Stage 2, we develop a suite of intermediate tools to move away from hand-written machine code.
These tools are "disposable" utilities designed to bridge the gap toward a more complete system,
prioritizing ease of implementation over performance and safety.

First, we implement `asm.lisp <https://github.com/nineties/hex-to-compiler/blob/main/asm.lisp>`_, a basic assembler. It makes significant architectural compromises to keep the implementation simple:

* **Target:** x86 32-bit mode only.
* **Direct Output:** It generates standalone executables directly, bypassing separate compilation.
* **Monolithic ELF:** All headers and code/data are packed into a single ``PT_LOAD (RWX)`` segment to avoid complex page alignment logic.

Example: `hello.s <https://github.com/nineties/hex-to-compiler/blob/main/asm/examples/hello.s>`_

Second, we implement `sasm.lisp <https://github.com/nineties/hex-to-compiler/blob/main/sasm.lisp>`_ (Structured Assembly).
This is a C-like untyped language implemented as an internal DSL within plisp.
To simplify development, it generates code for a stack-machine model using frequent push/pop instructions instead of complex register allocation.

Example: `hello.sv <https://github.com/nineties/hex-to-compiler/blob/main/sasm/examples/hello.sv>`_

Finally, I re-implemented ``plisp`` using ``sasm`` to create ``plisp2``. The performance of the initial plisp (built from hand-written machine code via Forth) was insufficient for complex tasks. While memory usage remains significant, ``plisp2`` provides the minimum efficiency required to proceed with the implementation of a full-scale language environment.

Hot to Run
----------

For x86 Linux
~~~~~~~~~~~~~

::

   $ git clone https://github.com/nineties/hex-to-compiler.git
   $ cd hex-to-compiler
   $ make

For Other Environments
~~~~~~~~~~~~~~~~~~~~~~

TBD

Development Blog
----------------

*Language: Japanese*

* `機械語手書きから言語処理系をブートストラップする <https://qiita.com/9_ties/items/349b2ed65b7cd8a7d580>`_
* `機械語手書きから言語処理系をブートストラップする(2) <https://qiita.com/9_ties/items/d70390d2a7f18b45b4d3>`_
* `機械語手書きから言語処理系をブートストラップする(3) <https://qiita.com/9_ties/items/689071cf51f1a8c15507>`_

Status
------

Ongoing personal work. No guarantees of completeness, correctness, or stability.

License
-------

See the LICENSE file.
