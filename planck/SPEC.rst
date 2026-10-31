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
   * - ``record T(f, ...)``
     - ``Apply{record_declare, Tuple{Quote{T}, Quote{Tuple{f, ...}}}}``
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
   * - ``a :: b``
     - ``Cons{a, b}``
   * - ``a + b`` ``a - b`` ``a * b`` ``a / b`` ``a % b``
     - ``Apply{add, Tuple{a, b}}`` and ``sub`` ``mul`` ``div`` ``mod``
   * - ``-a``
     - ``Apply{sub, Tuple{0, a}}``
   * - ``'e``, :literal:`\`e`, ``!e``
     - ``Quote{e}``, ``QuasiQuote{e}``, ``UnQuote{e}``
   * - ``f(a, ...)``
     - ``Apply{f, Tuple{a, ...}}``
   * - ``t.f``
     - ``Apply{record_get, Tuple{t, Quote{f}}}``
   * - ``p @ type``
     - ``_TypeOf{p, type}``
   * - ``{ e; ... }``
     - ``Do{e, ...}``
   * - ``if (c) a else b``, ``if (c) a``
     - ``If{c, a, b}``, ``If{c, a, none}``
   * - ``while (c) e``
     - ``While{c, e}``
   * - ``for (p in e) a``
     - ``Apply{peg_for, Tuple{e, Lambda{Tuple{p}, a}}}``
   * - ``return e``, ``return``, ``break``
     - ``Perform{Return{e}}``, ``Perform{Return{none}}``, ``Perform{Break{}}``
   * - ``match e { p -> a; ... }``
     - ``Apply{Fusion{Lambda{Tuple{p}, a}, ...}, Tuple{e}}``
   * - ``match e { p if c -> a; ... }``
     - the arm applies when ``p`` matches and ``c`` is true
   * - ``"a\(e)b"``
     - ``Apply{strcat, Tuple{Apply{strcat, Tuple{"a", Apply{to_text, Tuple{e}}}}, "b"}}``
   * - ``handle e with h``
     - ``Handle{e, h}``
   * - ``perform e``
     - ``Perform{e}``
   * - ``()``, ``(a, b, ...)``
     - ``Tuple{}``, ``Tuple{a, b, ...}``
   * - ``[a, b, ...]``, ``[]``
     - ``Cons{a, Cons{b, ... Nil{}}}``, ``Nil{}``

The rows are ordered from the loosest binding to the tightest.
``|``, ``->`` and ``::`` group to the right, ``+ - * / %`` group to the left
and comparisons do not group. ``::`` binds tighter than comparisons and looser than ``+ -``,
so ``a + 1 :: b == c`` is ``Cons{a + 1, b} == c``. ``()`` groups an expression.

The condition of ``if`` and ``while`` is in parentheses, and the body ``a``, ``b``, ``e`` is any expression;
a block ``{ ... }`` is the usual body of several expressions.
``else`` belongs to the nearest ``if`` that has no ``else``, so ``if (a) if (b) x else y`` is ``if (a) { if (b) x else y }``.
``else if (c) ...`` is ``else`` followed by an ``if`` expression.
An ``if`` is an expression: ``y = if (c) 1 else 2``.

``record T(f, g, ...)`` declares the names of the fields of the values ``T{a, b, ...}``, which are built as before,
and ``t.f`` is the argument of ``t`` at the position of the field ``f`` in the declaration for the head of ``t``.
Values with different heads can have fields of the same name. Declaring ``T`` again replaces the names.
A ``.`` follows its left operand with no space. A value whose head is not declared or has no field ``f`` stops the program with a message.

``for (p in e) a`` evaluates ``a`` for each element of the list or the vector ``e``, with the elements matched by the pattern ``p``,
and has the value ``none``. The variables of ``a`` outside the loop are shared, so ``a`` can assign them.

``return e`` ends the call of the function that contains it, with the value ``e`` (``none`` without ``e``; ``e`` must start on the same line).
``break`` ends the innermost ``while`` or ``for`` that contains it. They are effects:
the function is translated to ``Handle{body, (Return{v}, k) -> v}`` when its body has a ``return``,
and the loop to ``Handle{loop, (Break{}, k) -> none}`` when its body has a ``break``.
A ``return`` in a ``for`` body or in an arm of ``match`` belongs to the function around them,
and one in a function written inside belongs to that function.

``match`` evaluates ``e`` once and selects the first arm whose pattern ``p`` matches it.
The arms are separated by ``;`` or newlines. The body ``a`` is any expression.
The guard ``if c`` is evaluated after ``p`` matched, with the variables of ``p``; when it is false the arms after it are tried.
``e`` and ``c`` are expressions up to ``?:``, so a lambda needs parentheses.
When no arm applies the call fails as a call of a function with no matching pattern does.
``match`` binds ``%v`` and ``%r0``, ``%r1`` ... in the arms with guards, which no identifier can name.

Parameters ``p`` are patterns: unary expressions such as ``x``, ``1``, ``'none``, ``Cons{x, xs}`` or ``x @ int``,
and ``p :: q`` for ``Cons{p, q}``, e.g. ``(x :: xs) -> ...`` and ``f(x :: y :: rest) := ...``.

Lexical rules:

* identifiers are ``[A-Za-z_][A-Za-z0-9_]*`` except the reserved words
  ``if else while and or handle with perform match record for in return break``.
* integers are decimal (``10``, ``-3``), ``0x1f``, ``0o17`` or ``0b101``.
* strings are ``"..."`` in one line with the escapes ``\a \b \t \n \v \f \r \" \' \\``.
  ``\(e)`` in a string embeds the value of the expression ``e``, which may contain strings.
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
   * - ``array``
     - Mutable array made by ``array(n)``

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
     - ``strlen`` ``ord`` ``substr`` ``strcat`` ``intern`` ``symname``
   * - Arrays
     - ``array`` ``array_get`` ``array_set`` ``array_len`` ``hash``
   * - Files
     - ``read_file``
   * - Language
     - ``parse`` ``eval`` ``exit``
   * - ``planck/init.pk``
     - ``true`` ``false`` ``none`` ``not`` ``load`` ``use`` ``table`` ``table_get`` ``table_set``
   * - ``planck/peg.pk``
     - ``peg_parse`` ``define_rule``
   * - ``planck/syntax.pk``
     - ``parse_mexpr`` ``to_text`` ``record_declare`` ``record_get`` ``peg_for``

* ``parse_mexpr(s)``: the parser of M-expressions, with the same arguments and result as ``parse``.
* ``to_text(x)``: ``x`` for a string, the decimal text for an integer, and ``show(x)`` (``planck/lib/pretty.pk``) otherwise,
  so a value other than a string or an integer in ``\(e)`` needs ``use("pretty")``.
  A string with embedded values is joined by ``strcat`` from the left, and an empty piece is left out.
* ``record_declare(name, fields)``: set the field names of the head ``name`` (``fields`` is ``Tuple{f, ...}``). ``record_get(t, name)``: the field.
* ``peg_for(xs, f)``: call ``f`` with each element of the list or vector ``xs``, and return ``none``.
* ``symname(sym)``: the name of the symbol as a string (the inverse of ``intern``).
* ``strlen(s)``: the number of bytes. ``ord(s, i)``: the byte at ``i``, or ``-1`` when ``i`` is out of range.
* ``substr(s, i, j)``: the bytes from ``i`` to ``j - 1``. ``strcat(a, b)``: ``a`` followed by ``b``.
* ``intern(s)``: the symbol named ``s``.
* ``push(Head{a, ...}, x)``: ``Head{a, ..., x}``.
* ``array(n)``: a new array of ``n`` elements, all ``none``. ``array_get(a, i)``, ``array_set(a, i, x)`` (returns ``x``) and ``array_len(a)``.
  An index outside ``0`` to ``n - 1`` is a runtime error.
* ``hash(x)``: a non-negative integer. Values equal by ``eq`` have the same hash.
* ``table()``: a new hash table, made of arrays. ``table_get(t, key, default)``, ``table_set(t, key, value)``.
  Keys are compared by ``eq``.
* ``read_file(path)``: the contents of the file. ``load(path)``: parse and evaluate the file.
* ``define_rule(name, e)``: define the rule by a parsing expression (see ``planck/peg.pk``).
* ``use(name)``: ``load("planck/lib/<name>.pk")`` once.

Libraries
---------

The files of ``planck/lib`` are written in the syntax above and are loaded by ``use("name")``.
Each file uses the files it needs. Lists are ``Cons{x, xs}`` and ``Nil{}``, and a character is the integer that ``ord`` returns.

.. list-table::
   :header-rows: 1

   * - Name
     - Functions
   * - ``base``
     - ``panic(msg)`` prints ``msg`` and stops with status 1, ``assert(c, msg)``
   * - ``list``
     - ``length`` ``reverse`` ``append`` ``concat`` ``map`` ``filter`` ``concat_map`` ``each`` ``foldl`` ``foldr`` ``sum``
       ``nth`` ``last`` ``take`` ``drop`` ``find`` ``any`` ``all`` ``member`` ``index_of`` ``zip`` ``range`` ``sort``
   * - ``vector``
     - ``vec_new`` ``vec_push`` ``vec_pop`` ``vec_get`` ``vec_set`` ``vec_len`` ``vec_to_list`` ``vec_from_list``
   * - ``string``
     - ``is_digit`` ``is_upper`` ``is_lower`` ``is_alpha`` ``is_alnum`` ``is_space`` ``is_ident_start`` ``is_ident_char``
       ``chr`` ``char_at`` ``starts_with`` ``ends_with`` ``index_of_str`` ``str_repeat`` ``str_cmp`` ``str_less`` ``trim``
       ``int_to_str`` ``str_to_int`` ``split`` ``join`` ``strbuf_new`` ``strbuf_add`` ``strbuf_str``
   * - ``table``
     - ``table_len`` ``table_has`` ``table_keys`` ``table_values`` ``table_items``
       ``set_new`` ``set_add`` ``set_has`` ``set_len`` ``set_to_list`` ``set_from_list``
   * - ``symbol``
     - ``gensym`` ``gensym_reset``
   * - ``error``
     - ``line_col`` ``line_text`` ``loc_at`` ``show_loc`` ``error_at`` ``catch_error`` ``show_error``
   * - ``pretty``
     - ``quote_str`` ``show`` ``pretty`` ``pprint``

List functions:

* ``map(f, xs)``, ``filter(p, xs)`` and ``each(f, xs)`` call ``f`` from the first element to the last.
* ``foldl(f, acc, xs)`` computes ``f(f(acc, x1), x2)``; ``foldr(f, init, xs)`` computes ``f(x1, f(x2, init))``.
* ``nth(xs, i)`` counts from 0. ``take(xs, n)`` and ``drop(xs, n)`` split ``xs`` after ``n`` elements.
* ``find(p, xs)`` is the first element with ``p(x)`` or ``none``. ``index_of(x, xs)`` is the position or ``-1``.
* ``zip(xs, ys)`` is ``[Tuple{x, y}, ...]`` as long as the shorter list. ``range(a, b)`` is ``[a, ..., b - 1]``.
* ``sort(less, xs)`` is a stable merge sort by ``less(a, b)``, e.g. ``sort(lt, xs)``.

A vector is a growable array: ``vec_push(v, x)`` appends and returns ``x``, ``vec_pop(v)`` removes and returns the last element.
``vec_get`` and ``vec_set`` stop the program for an index outside the elements.

String functions:

* ``chr(c)`` is the one-character string of a code from 32 to 126, 9, 10 or 13. ``char_at(s, i)`` is ``""`` outside ``s``.
* ``index_of_str(s, part, from)`` is the first position at or after ``from`` where ``part`` occurs, or ``-1``.
* ``str_cmp(a, b)`` is ``-1``, ``0`` or ``1`` by the byte order. ``trim(s)`` removes blanks at both ends.
* ``str_to_int(s)`` reads an optional ``-`` and decimal digits, otherwise ``none``.
* ``split(s, sep)`` is the list of the parts between the occurrences of ``sep``, which must not be empty.
  ``join(sep, xs)`` is the inverse.
* A string buffer collects pieces: ``strbuf_add(b, s)`` and ``strbuf_str(b)``.
  The pieces are joined pairwise, so a long string is built without copying the same bytes over and over.

``table_keys``, ``table_values`` and ``table_items`` (``[Tuple{key, value}, ...]``) list the entries in an unspecified order.
A set is a table whose values are ``true``.

``gensym("t")`` is the symbol ``t1``, then ``t2``, ``t3`` ... ``gensym_reset()`` restarts the count.

Errors with positions:

* ``line_col(src, pos)`` is ``Tuple{line, column}`` of the byte position ``pos``; both count from 1.
  ``line_text(src, line)`` is the line without the newline.
* ``loc_at(file, src, pos)`` is ``Loc{file, line, column}``, and ``show_loc`` writes it as ``file:line:column``.
* ``error_at(loc, msg)`` performs ``Error{loc, msg}``. ``catch_error(f)`` is ``Ok{f()}``,
  or ``Err{loc, msg}`` when ``f()`` performed ``Error{loc, msg}``.
* ``show_error(src, loc, msg)`` is ``file:line:column: msg``, the source line and a ``^`` under the column.

``show(x)`` is the text that ``print`` writes, with ``[a, b]`` for lists. ``pretty(x, width)`` keeps a part that fits in ``width``
columns on one line and puts every element of the others on its own line, indented by 2 spaces. ``pprint(x)`` prints it with width 60.
