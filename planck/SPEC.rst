Planck Language Specification
=============================

Planck is a language for symbolic and language processing.
``planck/init.pk`` is loaded first, then the given files are run in order.
Each top-level expression is parsed and evaluated one by one.

``planck/init.pk``, ``planck/peg.pk`` and ``planck/syntax.pk`` are written in M-expressions.
``planck/syntax.pk`` defines the syntax below with the packrat parser of ``planck/peg.pk``
and replaces ``parse``, so the given files are read by the syntax below.

M-expressions
-------------

Symbols (``x``), integers (``42``, ``-1``), strings (``"abc"``) and ``Head{a, b, ...}``.
A string used as a top-level expression has no effect, so it serves as a comment.

Syntax
------

An expression is translated into an M-expression, which is evaluated as usual.
M-expressions are expressions of this syntax too.

.. list-table::
   :header-rows: 1

   * - Expression
     - M-expression
   * - ``x := e``
     - ``Def{x, e}``
   * - ``f(p, ...) := e``
     - ``Def{f, Lambda{Tuple{p, ...}, e}}``
   * - ``x = e``
     - ``Set{x, e}``
   * - ``f | g``
     - ``Fusion{f, g}``
   * - ``(p, ...) -> e``, ``p -> e``
     - ``Lambda{Tuple{p, ...}, e}``
   * - ``c ? a : b``
     - ``If{c, a, b}``
   * - ``a or b``, ``a and b``
     - ``If{a, true, b}``, ``If{a, b, false}``
   * - ``not a``
     - ``Apply{not, Tuple{a}}``
   * - ``a == b`` ``a != b`` ``a < b`` ``a <= b`` ``a > b`` ``a >= b``
     - ``Apply{eq, Tuple{a, b}}`` and ``ne`` ``lt`` ``le`` ``gt`` ``ge``
   * - ``a + b`` ``a - b`` ``a * b`` ``a / b`` ``a % b``
     - ``Apply{add, Tuple{a, b}}`` and ``sub`` ``mul`` ``div`` ``mod``
   * - ``-a``
     - ``Apply{sub, Tuple{0, a}}``
   * - ``'e``, :literal:`\`e`, ``!e``
     - ``Quote{e}``, ``QuasiQuote{e}``, ``UnQuote{e}``
   * - ``f(a, ...)``
     - ``Apply{f, Tuple{a, ...}}``
   * - ``p @ type``
     - ``_TypeOf{p, type}``
   * - ``{ e; ... }``
     - ``Do{e, ...}``
   * - ``if c { ... } else { ... }``, ``if c { ... }``
     - ``If{c, Do{...}, Do{...}}``, ``If{c, Do{...}, none}``
   * - ``while c { ... }``
     - ``While{c, Do{...}}``
   * - ``handle e with h``
     - ``Handle{e, h}``
   * - ``perform e``
     - ``Perform{e}``
   * - ``()``, ``(a, b, ...)``
     - ``Tuple{}``, ``Tuple{a, b, ...}``
   * - ``[a, b, ...]``, ``[]``
     - ``Cons{a, Cons{b, ... Nil{}}}``, ``Nil{}``

The rows are ordered from the loosest binding to the tightest.
``|`` and ``->`` group to the right, ``+ - * / %`` group to the left
and comparisons do not group. ``()`` groups an expression.
``else if c { ... }`` is ``else { if c { ... } }``.
Parameters ``p`` are patterns: unary expressions such as ``x``, ``1``, ``'none``, ``Cons{x, xs}`` or ``x @ int``.

Lexical rules:

* identifiers are ``[A-Za-z_][A-Za-z0-9_]*`` except the reserved words
  ``if else while and or handle with perform``.
* integers are decimal (``10``, ``-3``), ``0x1f``, ``0o17`` or ``0b101``.
* strings are ``"..."`` in one line with the escapes ``\a \b \t \n \v \f \r \" \' \\``.
* ``#`` starts a comment up to the end of the line.
* there is no space between ``f`` and ``(`` in ``f(a)``, and between ``Head`` and ``{`` in ``Head{a}``.

Expressions in a block are separated by ``;`` or newlines.
A top-level expression ends with ``;``, a newline or the start of another expression.
A binary ``-`` must be on the same line as its left operand,
so a line starting with ``-1`` is a new expression. Other binary operators may start a line.

Grammar declarations
--------------------

``name ::= alternative | ...`` defines or replaces the rule ``name`` of the grammar.
The rules of the syntax above (``expr``, ``primary``, ``block``, ``add_expr`` ...,
``ident``, ``integer``, ``string``) are rules of the same grammar,
so a declaration can extend the syntax for the following expressions.
``...`` as an alternative stands for the previous definition of the rule.

.. code-block::

   primary ::= 'unless c:expr a:block 'else b:block { `If{!c, !b, !a} } | ...

An alternative is a sequence of items followed by an optional action ``{ ... }``.
The items and the action are on the same line as ``::=`` or ``|``.

.. list-table::
   :header-rows: 1

   * - Item
     - Matches
   * - ``"text"``
     - the text, after blanks and comments
   * - ``'word``
     - the word, not followed by ``[A-Za-z0-9_]``
   * - ``name``
     - the rule
   * - ``(a | b ...)``
     - a choice
   * - ``e*``, ``e+``, ``e?``
     - zero or more, one or more, optional
   * - ``&e``, ``!e``
     - lookahead: ``e`` matches / does not match, without consuming
   * - ``label:e``
     - ``e``, and its value is the variable ``label`` in the action

Alternatives are tried in order (PEG). The value of an alternative is the value of its action,
the value of the item when there is one item, or ``Tuple{v, ...}`` of the items.
``e*`` and ``e+`` give ``Tuple{v, ...}``, ``e?`` gives ``Tuple{}`` or ``Tuple{v}``.
Results of rules are memoized for each position (packrat parsing).
A rule may be directly left recursive (``sum ::= a:sum "+" b:term { ... } | term``), which groups to the left.

``peg_parse(rule, s)`` parses a prefix of ``s`` by the rule and returns ``Tuple{value, rest}``,
``Tuple{Eof{}, ""}`` when only blanks are left, or ``Tuple{SyntaxError{...}, ""}``.

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
   * - ``table``
     - Mutable table made by ``table()``

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
     - ``head`` ``arity`` ``arg`` ``mexpr`` ``push`` ``typeof``
   * - Strings
     - ``strlen`` ``ord`` ``substr`` ``strcat`` ``intern``
   * - Tables
     - ``table`` ``table_get`` ``table_set``
   * - Files
     - ``read_file``
   * - Language
     - ``parse`` ``eval`` ``exit``
   * - ``planck/init.pk``
     - ``true`` ``false`` ``none`` ``not`` ``load``
   * - ``planck/peg.pk``
     - ``peg_parse`` ``define_rule``
   * - ``planck/syntax.pk``
     - ``parse_mexpr`` (the parser of M-expressions)

* ``strlen(s)``: the number of bytes. ``ord(s, i)``: the byte at ``i``, or ``-1`` when ``i`` is out of range.
* ``substr(s, i, j)``: the bytes from ``i`` to ``j - 1``. ``strcat(a, b)``: ``a`` followed by ``b``.
* ``intern(s)``: the symbol named ``s``.
* ``push(Head{a, ...}, x)``: ``Head{a, ..., x}``.
* ``table()``: a new table. ``table_get(t, key, default)``, ``table_set(t, key, value)``.
  Keys are compared by ``eq``.
* ``read_file(path)``: the contents of the file. ``load(path)``: parse and evaluate the file.
* ``define_rule(name, e)``: define the rule by a parsing expression (see ``planck/peg.pk``).
