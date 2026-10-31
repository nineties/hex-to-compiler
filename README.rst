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
`planck.xxd <https://github.com/nineties/hex-to-compiler/blob/main/pforth.xxd>`_

Using Forth’s self-extensibility, this interpreter is grown into a more capable
language environment, eventually producing a simple Lisp interpreter.
`plisp.fs <https://github.com/nineties/hex-to-compiler/blob/main/plisp.fs>`_

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

Finally, I re-implemented ``plisp`` using ``sasm`` to create ``plisp2``. 
`plisp2.sv <https://github.com/nineties/hex-to-compiler/blob/main/plisp2.sv>`_

The performance of the initial plisp (built from hand-written machine code via Forth) was insufficient for complex tasks. While memory usage remains significant, ``plisp2`` provides the minimum efficiency required to proceed with the implementation of a full-scale language environment.

Stage3: Implementing Full-Scale Language Processor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Stage3 starts with ``planck0``, a Planck interpreter written in ``sasm``.
`planck0.sv <https://github.com/nineties/hex-to-compiler/blob/main/planck0.sv>`_

``planck0`` reads ``planck/init.pk``, then runs the files given on the command line in order.
Each top-level expression is parsed and evaluated one by one.

**Syntax**

M-expressions only: symbols (``x``), integers (``42``, ``-1``), strings (``"abc"``) and ``Head{a, b, ...}``.
A string used as a top-level expression has no effect, so it serves as a comment.

**Values**

.. list-table::
   :header-rows: 1

   * - ``typeof``
     - Value
   * - ``int``
     - Integer
   * - ``string``
     - String
   * - ``symbol``
     - Symbol (``true``, ``false`` and ``none`` are symbols defined in ``planck/init.pk``)
   * - ``mexpr``
     - ``Head{a, ...}``
   * - ``function``
     - Lambda, Fusion, builtin function or continuation

**Special forms**

.. list-table::
   :header-rows: 1

   * - Form
     - Meaning
   * - ``Def{x, e}`` / ``Set{x, e}``
     - Define / assign a variable
   * - ``Apply{f, Tuple{a, ...}}``
     - Call ``f``
   * - ``Do{e, ...}``
     - Evaluate in order, return the last
   * - ``If{c, a, b}``
     - ``b`` when ``c`` is ``false``, otherwise ``a``
   * - ``While{c, e}``
     - Loop
   * - ``Lambda{Tuple{pattern, ...}, body}``
     - Function
   * - ``Fusion{f, g}``
     - Try ``f``, then ``g`` if no pattern of ``f`` matches
   * - ``Quote{e}``, ``QuasiQuote{e}``, ``UnQuote{e}``
     - Quotation
   * - ``Handle{body, handler}``, ``Perform{v}``
     - Algebraic effects
   * - ``Tuple{a, ...}``
     - Tuple

An M-expression with any other head, such as ``Cons{x, y}``, is a constructor:
its arguments are evaluated and an M-expression with the same head is built.

**Patterns**

A parameter pattern is one of:

* a variable ``x``, or ``_`` (matches anything and binds nothing)
* an integer or a string (matches an equal value)
* ``Quote{v}`` (matches an equal value, e.g. ``Quote{false}``)
* ``Head{p, ...}`` (matches an M-expression with the same head and arity)
* ``_TypeOf{p, type}`` (``p`` matches and the value has the type shown by ``typeof``)

**Algebraic effects**

``Handle{body, handler}`` evaluates ``body``. ``Perform{v}`` calls the nearest ``handler(v, k)`` that has a matching pattern,
where ``k`` is the continuation of the ``Perform``. Handlers are deep: they stay installed in the continuation.
When no pattern matches, the effect goes to the outer handler.
``k`` is an ordinary function and can be called any number of times, even after ``Handle`` has returned.
Variables are shared between the resumed copies.

**Top level**

The loop looks up the global variables ``parse`` and ``eval`` for every expression,
so a program can replace them and the next expression is handled by the new definitions.
``Apply{eval, ...}`` in a program also calls the replaced ``eval``.

* ``parse``: ``String -> Tuple{m-expr, rest of the string}``.
  It skips blanks by itself and returns ``Tuple{Eof{}, ""}`` when only blanks are left.
  A replacement ``parse`` must pass ``Eof{}`` through.
* ``eval``: ``m-expr -> value``.

**Builtin functions**

.. list-table::
   :header-rows: 1

   * - Group
     - Functions
   * - Output
     - ``print`` ``println`` ``put``
   * - Integers
     - ``add`` ``sub`` ``mul`` ``div`` ``mod``
   * - Comparison
     - ``lt`` ``le`` ``gt`` ``ge`` ``eq`` ``ne``
   * - M-expressions
     - ``head`` ``arity`` ``arg`` ``mexpr`` ``typeof``
   * - Language
     - ``parse`` ``eval`` ``exit``
   * - ``planck/init.pk``
     - ``true`` ``false`` ``none`` ``not``

Examples: `planck/examples <https://github.com/nineties/hex-to-compiler/tree/main/planck/examples>`_
(`reader.pk <https://github.com/nineties/hex-to-compiler/blob/main/planck/examples/reader.pk>`_ replaces ``parse`` and ``eval``)

Hot to Run
----------

For x86 Linux
~~~~~~~~~~~~~

::

   $ git clone https://github.com/nineties/hex-to-compiler.git
   $ cd hex-to-compiler
   $ make
   $ make test-planck0   # planck/test.pk and planck/examples

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
