Planck Language Specification
=============================

Planck is a language for symbolic and language processing.
``planck0`` (`planck0.sv <https://github.com/nineties/hex-to-compiler/blob/main/planck0.sv>`_)
is an implementation of this specification.

``planck0`` reads ``planck/init.pk``, then runs the files given on the command line in order.
Each top-level expression is parsed and evaluated one by one.

Syntax
------

M-expressions only: symbols (``x``), integers (``42``, ``-1``), strings (``"abc"``) and ``Head{a, b, ...}``.
A string used as a top-level expression has no effect, so it serves as a comment.

Values
------

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

Special forms
-------------

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

Patterns
--------

A parameter pattern is one of:

* a variable ``x``, or ``_`` (matches anything and binds nothing)
* an integer or a string (matches an equal value)
* ``Quote{v}`` (matches an equal value, e.g. ``Quote{false}``)
* ``Head{p, ...}`` (matches an M-expression with the same head and arity)
* ``_TypeOf{p, type}`` (``p`` matches and the value has the type shown by ``typeof``)

Algebraic effects
-----------------

``Handle{body, handler}`` evaluates ``body``. ``Perform{v}`` calls the nearest ``handler(v, k)`` that has a matching pattern,
where ``k`` is the continuation of the ``Perform``. Handlers are deep: they stay installed in the continuation.
When no pattern matches, the effect goes to the outer handler.
``k`` is an ordinary function and can be called any number of times, even after ``Handle`` has returned.
Variables are shared between the resumed copies.

Top level
---------

The loop looks up the global variables ``parse`` and ``eval`` for every expression,
so a program can replace them and the next expression is handled by the new definitions.
``Apply{eval, ...}`` in a program also calls the replaced ``eval``.

* ``parse``: ``String -> Tuple{m-expr, rest of the string}``.
  It skips blanks by itself and returns ``Tuple{Eof{}, ""}`` when only blanks are left.
  A replacement ``parse`` must pass ``Eof{}`` through.
* ``eval``: ``m-expr -> value``.

Builtin functions
-----------------

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
