Planck Language Specification
=============================

Planck is a language for symbolic and language processing.
``planck/init.pk`` is loaded first, then the program file given as the first argument is run.
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
     - ``Do{Declare{f}, Set{f, Fusion{f, Lambda{Tuple{p, ...}, e}}}}``
   * - ``record T(f, ...)``
     - ``Apply{_record_declare, Tuple{Quote{T}, Quote{Tuple{f, ...}}}}``
   * - ``x = e``
     - ``Set{x, e}``
   * - ``a[i] = e``
     - ``Apply{_index_set, Tuple{a, i, e}}``
   * - ``f | g``
     - ``Fusion{f, g}``
   * - ``(p, ...) if c -> e | g``, ``p if c -> e | g``
     - a function that calls ``g`` with its arguments when they do not match the parameters or ``c`` is false
   * - ``(p, ...) -> e``, ``p -> e``
     - ``Lambda{Tuple{p, ...}, e}``
   * - ``c ? a : b``
     - ``If{c, a, b}``
   * - ``a or b``, ``a and b``
     - ``If{a, true, b}``, ``If{a, b, false}``
   * - ``not a``
     - ``Apply{_not, Tuple{a}}``
   * - ``a == b`` ``a != b`` ``a < b`` ``a <= b`` ``a > b`` ``a >= b``
     - ``Apply{_eq, Tuple{a, b}}`` and ``_ne`` ``_lt`` ``_le`` ``_gt`` ``_ge``
   * - ``a :: b``
     - ``Cons{a, b}``
   * - ``a + b`` ``a - b`` ``a * b`` ``a / b`` ``a % b``
     - ``Apply{_add, Tuple{a, b}}`` and ``_sub`` ``_mul`` ``_div`` ``_mod``
   * - ``-a``
     - ``Apply{_sub, Tuple{0, a}}``
   * - ``'e``, :literal:`\`e`, ``!e``
     - ``Quote{e}``, ``QuasiQuote{e}``, ``UnQuote{e}``
   * - ``f(a, ...)``
     - ``Apply{f, Tuple{a, ...}}``
   * - ``t.f``
     - ``Apply{_record_get, Tuple{t, Quote{f}}}``
   * - ``a[i]``
     - ``Apply{_index_get, Tuple{a, i}}``
   * - ``t[p -> a | q if c -> b]``
     - ``Apply{_index_get, Tuple{t, f}}``, where ``f`` is the function of the arms (see Indexing and rewriting)
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
   * - ``return e``, ``return``, ``break``, ``continue``
     - ``Perform{Return{e}}``, ``Perform{Return{none}}``, ``Perform{Break{}}``, ``Perform{Continue{}}``
   * - ``match e { f }``
     - ``Apply{f, Tuple{e}}``
   * - ``f"a{e}b"``
     - ``Apply{_strcat, Tuple{Apply{_strcat, Tuple{"a", Apply{_to_text, Tuple{e}}}}, "b"}}``
   * - ``fun f``
     - ``Def{f, none}``
   * - ``f(p, ...) = e``
     - ``Set{f, Fusion{f, Lambda{Tuple{p, ...}, e}}}``
   * - ``handle e with h``
     - ``Handle{e, h}``
   * - ``perform e``
     - ``Perform{e}``
   * - ``()``, ``(a, b, ...)``
     - ``Tuple{}``, ``Tuple{a, b, ...}``
   * - ``[a, b, ...]``, ``[]``
     - ``Cons{a, Cons{b, ... Nil{}}}``, ``Nil{}``
   * - ``name{| text |}``
     - ``Quote{v}``, where ``v`` is the value of the rule ``name`` for ``text`` (see Text of a rule)

The rows are ordered from the loosest binding to the tightest.
``|``, ``->`` and ``::`` group to the right, ``+ - * / %`` group to the left
and comparisons do not group. ``::`` binds tighter than comparisons and looser than ``+ -``,
so ``a + 1 :: b == c`` is ``Cons{a + 1, b} == c``. ``()`` groups an expression.

The condition of ``if`` and ``while`` is in parentheses. The body ``e`` of ``while`` is any expression,
and the branches ``a`` and ``b`` of ``if`` are expressions without ``|`` outside of brackets,
so ``if (c) a else b | g`` is ``(if (c) a else b) | g``. A block ``{ ... }`` is the usual body of several expressions.
``else`` belongs to the nearest ``if`` that has no ``else``, so ``if (a) if (b) x else y`` is ``if (a) { if (b) x else y }``.
``else if (c) ...`` is ``else`` followed by an ``if`` expression.
An ``if`` is an expression: ``y = if (c) 1 else 2``.

``record T(f, g, ...)`` declares the names of the fields of the values ``T{a, b, ...}``, which are built as before,
and ``t.f`` is the argument of ``t`` at the position of the field ``f`` in the declaration for the head of ``t``.
Values with different heads can have fields of the same name. Declaring ``T`` again replaces the names.
A ``.`` follows its left operand with no space. A value whose head is not declared or has no field ``f`` stops the program with a message.

``a[i]`` and ``a[i] = e`` are described in Indexing and rewriting. The ``[`` follows ``a`` with no space,
so ``a`` followed by ``[1, 2]`` on the next line or after a space is two expressions.
``a`` is a postfix expression such as ``x``, ``f(x)``, ``t.f`` or ``b[j]``: ``a[i][j] = e`` is ``Apply{_index_set, Tuple{a[i], j, e}}``.

Variables have lexical scope, and the variable a name refers to is decided before the expression runs.
A variable in the body of a function is a parameter of that function or of a function around it,
or a variable defined before it in those functions, and otherwise a variable defined before it at the top level (a global variable).
The variables of the caller are not visible. If a name refers to no variable, the program stops with a message before the expression runs.
A function keeps the variables of the place where it was made,
and they are shared locations: an assignment to them after the function was made is seen by the function.

The top-level definitions are resolved in the order they are written. A definition written later is not visible to the expressions before it.
The body of a function definition ``f(p, ...) := e`` sees ``f`` itself. Defining the same name again with ``x := e`` or ``fun f`` makes a new variable.
The functions made before keep referring to the old variable, and only the expressions after the definition see the new one.
With ``f(x) := 1``, ``g() := f(0)`` and ``f := (x) -> 2`` in this order, ``g()`` is ``1``.
To overwrite an existing variable, write ``x = e``. It is an assignment to the same variable,
so the functions made before also see the new value. With ``f = (x) -> 2`` instead of ``f := (x) -> 2``, ``g()`` is ``2``.

A function definition ``f(p, ...) := e`` is ``f(p, ...) = e`` when ``f`` is already a variable of the place where it is written,
and otherwise ``fun f`` followed by ``f(p, ...) = e``, which defines a new ``f`` where it is written.
The variables of the place are the global variables defined by the same file at the top level,
and in a function the parameters of the function and the variables defined before it in the function.
A global variable defined by another file, such as a library loaded by ``use`` or ``planck/init.pk``, or a builtin function, is not of the place,
so ``f(p, ...) := e`` at the top level of a program defines a new ``f`` that hides it.
A variable of a function around it, or a global variable seen from a function, is not of the place either,
so ``f(p, ...) := e`` there defines a new ``f`` and does not change the ``f`` outside.
In a branch of ``if`` or the body of ``while`` at the top level, the variables of the place are those defined before it in the branch or the body.
Several definitions of ``f`` in one place make one function whose functions are tried in the order they are written::

   fib(0) := 0
   fib(1) := 1
   fib(n) := fib(n - 1) + fib(n - 2)

Functions that call each other first declare the name with ``fun f``.
``fun f`` defines a variable ``f`` whose value is ``none`` where it is written, and ``f(p, ...) = e`` later fills it.
``f(p, ...) = e`` is the assignment ``f = f | (p, ...) -> e``: it adds the function after the functions of ``f``,
so the functions are tried in the order they are written. ``none | g`` is ``g``,
so the first ``f(p, ...) = e`` after ``fun f`` assigns the function ``(p, ...) -> e`` itself.
The functions may have different numbers of parameters. The functions written after the declaration see ``f``.
Calling ``f`` before it is filled calls ``none`` and stops the program::

   fun odd
   even(n) := if (n == 0) true else odd(n - 1)
   odd(n) = if (n == 0) false else even(n - 1)

   fun fact
   fact(0) = 1
   fact(n) = n * fact(n - 1)   # fact(0) is 1

When the value of ``f`` is a function with a guard, as after ``f = (n) if n > 0 -> 1``,
a function added by ``f(p, ...) = e`` is not called when the guard is false, as ``h | g`` below.

A variable definition ``x := e`` (also ``x := (p) -> e``) is defined where it is written,
and the expressions before it see the ``x`` outside. The right-hand side ``e`` sees the previous ``x`` too.
The new ``x`` is made after the right-hand side is evaluated, so ``x`` in the right-hand side is undefined if there is no previous ``x``.
A function that calls itself is written as ``f(p, ...) := e``. This is the same at the top level and in a block::

   x := 1
   x := x + 1   # x is 2

A variable of a block is a variable of the function or the program that contains the block.

``for (p in e) a`` evaluates ``a`` for each element of the list or the vector ``e``, with the elements matched by the pattern ``p``,
and has the value ``none``. The variables of ``a`` outside the loop are shared, so ``a`` can assign them.

``return e`` ends the call of the function that contains it, with the value ``e`` (``none`` without ``e``; ``e`` must start on the same line).
``e`` is an expression up to ``->``, without ``|``, ``:=`` and ``=`` outside of brackets.
``break`` ends the innermost ``while`` or ``for`` that contains it. They are effects:
the function is translated to ``Handle{body, (Return{v}, k) -> v}`` when its body has a ``return``,
and the loop to ``Handle{loop, (Break{}, k) -> none}`` when its body has a ``break``.
``continue`` goes on to the next round of the innermost ``while`` or ``for`` that contains it:
the body of the loop is translated to ``Handle{body, (Continue{}, k) -> none}`` when it has a ``continue``.
A ``continue`` outside of a loop stops the program with ``unhandled effect: Continue{}``.
A ``return`` in a ``for`` body or in an arm of ``match`` belongs to the function around them,
and one in a function written inside belongs to that function.

``match e { f }`` evaluates ``e`` once and calls the function ``f`` with it.
``f`` is an expression up to ``|``, usually functions of one parameter joined by ``|``, the arms,
so the first arm whose pattern matches ``e`` is selected.
An arm takes one parameter: ``(a, b) -> x`` takes two, and ``((a, b)) -> x`` takes a tuple.
A ``|`` may start a line, and the arms after the first one are usually written so:

.. code-block::

   match e {
       p -> a
     | q -> b
   }

The guard ``if c`` of ``(p, ...) if c -> e | g`` is evaluated after the parameters matched, with their variables.
When it is false, or the arguments do not match the parameters, ``g`` is called with the same arguments,
so ``g`` is the arms after it. Without ``| g`` the call fails as a call of a function with no matching pattern does.
``g`` is what is joined after the function by ``|`` in the text, also when the function is in parentheses: ``(p if c -> a) | g``.
A function with a guard that is given by a variable, as ``h | g`` after ``h := p if c -> a``, does not call ``g``.
The guard and ``e`` of ``match`` are expressions up to ``?:``, so a lambda needs parentheses.
The function binds variables whose names start with ``%``, which no identifier can name.

Parameters ``p`` and the patterns of ``for`` are written as expressions up to ``or``,
and are the M-expressions of the table above, which are matched as in Patterns below, with these rules:

* the name of an operator or of a called function is a symbol: ``a + b`` is ``Apply{Quote{_add}, Tuple{a, b}}``,
  ``-a`` is ``Apply{Quote{_sub}, Tuple{0, a}}`` and ``f(p, ...)`` is ``Apply{Quote{f}, Tuple{p, ...}}``,
  so they match the trees of ``x + 1``, ``-x`` and ``f(1)``.
  ``a and b`` and ``a or b`` are ``If{a, b, Quote{false}}`` and ``If{a, Quote{true}, b}``.
* the other identifiers are variables. The arguments of ``Head{p, ...}`` are patterns,
  so ``Apply{f, Tuple{x}}`` matches a call of any function with one argument and binds ``f``.
* ``'e`` is ``Quote{e}`` for the expression ``e``.
* the forms are the operators ``or and not == != <= >= < > :: + - * / %``, the unary ``-`` and ``'``, ``f(p, ...)``,
  ``p @ type``, ``Head{p, ...}``, identifiers, integers, strings, ``()``, ``(p)``, ``(p, q, ...)`` and ``[p, ...]``,
  with the same binding as in expressions: ``a @ int + b @ int`` is ``(a @ int) + (b @ int)``.

.. code-block::

   simp(e) := match e {
       a @ int + b @ int -> a + b
     | a + 0 -> a
     | x * y if x == y -> `sq(!x)
     | _ -> e
   }
   simp('(1 + 2))    # 3
   simp('(z * z))    # sq(z)
   swap(a + b) := `(!b + !a)
   (x :: xs) -> x

Lexical rules:

* identifiers are ``[A-Za-z_][A-Za-z0-9_]*`` except the reserved words
  ``if else while and or handle with perform match record for in return break continue fun``.
* integers are decimal (``10``, ``-3``), ``0x1f``, ``0o17`` or ``0b101``.
* strings are ``"..."`` in one line with the escapes ``\a \b \t \n \v \f \r \" \' \\``.
  ``f"...{e}..."`` embeds the value of the expression ``e``, which may contain strings and f-strings;
  ``{{`` and ``}}`` are the braces themselves. A string without the prefix ``f`` embeds nothing.
* ``#`` starts a comment up to the end of the line.
* there is no space between ``f`` and ``(`` in ``f(a)``, between ``a`` and ``[`` in ``a[i]``, and between ``Head`` and ``{`` in ``Head{a}``.

Expressions in a block are separated by ``;`` or newlines.
A top-level expression ends with ``;``, a newline or the start of another expression.
A binary ``-`` must be on the same line as its left operand,
so a line starting with ``-1`` is a new expression. Other binary operators may start a line.

Names the sugar calls
---------------------

The functions a piece of sugar expands to have a name starting with ``_``,
and there is no function of the same name without ``_``.
A variable named ``add`` or ``index_get`` in a program therefore does not change the meaning of ``a + b`` or ``a[i]``.
The operators are ``_add`` ``_sub`` ``_mul`` ``_div`` ``_mod`` ``_lt`` ``_le`` ``_gt`` ``_ge`` ``_eq`` ``_ne`` ``_not``.
A string with embedded values uses ``_strcat`` and ``_to_text``, ``t.f`` uses ``_record_get``,
``record`` uses ``_record_declare`` and ``::=`` uses ``_define_rule``.
``a[i]`` and ``a[i] = e`` use ``_index_get`` and ``_index_set``, defined below.
A function that takes the operator as an argument is written with the sugar, e.g. ``sort((a, b) -> a < b, xs)``,
and a tree made by the sugar has these names: ``a + b`` is ``Apply{_add, Tuple{a, b}}``.

Indexing and rewriting
----------------------

``a[i]`` is ``_index_get(a, i)`` and ``a[i] = e`` is ``_index_set(a, i, e)``, whose value is the value of ``e``.
``_index_get`` and ``_index_set`` are functions joined by ``|``, one for each kind of ``a``, tried in this order:

.. list-table::
   :header-rows: 1

   * - ``a``
     - ``i``
     - ``a[i]``
     - ``a[i] = e``
   * - array
     - integer
     - ``array_get(a, i)``
     - ``array_set(a, i, e)``
   * - plain array
     - integer
     - ``plain_get(a, i)``
     - ``plain_set(a, i, e)``
   * - string
     - integer
     - the character at ``i``, a string of length 1
     - stops, see below
   * - table
     - any value
     - ``table_get(a, i, none)``: ``none`` for a missing key
     - ``table_set(a, i, e)``
   * - vector (``use("vector")``)
     - integer
     - ``vec_get(a, i)``
     - ``vec_set(a, i, e)``
   * - any value
     - function
     - ``subst(a, i)``, below
     -
   * - list
     - integer
     - the element at ``i``, counted from 0
     - stops, see below
   * - M-expression
     - symbol
     - ``_record_get(a, i)``, the field
     - stops, see below
   * - M-expression
     - integer
     - ``arg(a, i)``
     - stops, see below

A string, a list, a tuple and an M-expression are immutable:
``a[i] = e`` on them stops the program with the message ``cannot assign to an element of a string, which is immutable: ...``.
An array, a plain array, a table, a vector and a kind of a program (added in front) can be written.
An index outside a string, a list or the arguments of an M-expression stops the program with a message,
and so does ``a[i]`` or ``a[i] = e`` that no kind accepts.
A program adds a kind of its own in front of the others by assigning ``_index_get`` and ``_index_set``:

.. code-block::

   _index_get = (Matrix{w, _, d}, (i, j)) -> d[i * w + j] | _index_get
   _index_set = (Matrix{w, _, d}, (i, j), e) -> { d[i * w + j] = e } | _index_set
   m := Matrix{2, 2, array(4)}
   m[(1, 0)] = 5

``subst(t, f)`` rewrites the tree ``t`` with the function ``f``. Each part of ``t`` is tried from the root down:
when a pattern of ``f`` matches the part, the part is replaced by the value of ``f`` and the parts inside it are not tried;
otherwise the arguments of the part are tried and its head is kept. A part that is not an M-expression has no parts.

.. code-block::

   t := '(x * 1 + 0 * y)
   t[a * 1 -> a | 0 * _ -> 0]    # x + 0

``t[p -> a | q -> b]`` is the expression ``p -> a | q -> b`` in brackets, so its arms are tried in order.
An arm may have a guard as in ``match``: in ``t[p if c -> a | q -> b]`` the arm applies when ``p`` matches and ``c`` is true,
otherwise the next arm is tried, and a part that no arm applies to is gone into.
The body of an arm is an expression up to ``->``, so ``|`` in a body needs parentheses.
``'t[f]`` quotes ``t[f]``: a quoted tree to rewrite is written ``('e)[f]``.

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

Text of a rule
--------------

``name{| text |}`` is an expression whose text is read by the rule ``name`` when the expression is parsed.
There is no space between ``name`` and ``{|``.
The text is read only by the rule (its strings, words and rules, with their blanks and comments),
not by the tokens of the expressions around it.
The rule decides where the text ends, and ``|}`` must follow, after blanks.
So ``|}`` and ``}`` that the rule reads, for example in its strings or brackets, are a part of the text.
A failure of the rule or a missing ``|}`` is a syntax error of the expression,
at the farthest position the rule reached in the text.

The value of the expression is the value of the rule, as a constant.

.. code-block::

   calc ::= a:calc "+" b:num { a + b } | num
   num ::= integer
   calc{| 1 + 2 + 3 |}        # 6, computed when the line is parsed

The rule must be defined by an expression that is evaluated before the expression with the text is parsed.

``peg_sources`` is a table from a rule name to the parsing expression given to ``_define_rule``, with the actions not evaluated
(``...`` replaced by the previous definition). ``peg_version`` increases at every ``_define_rule``.

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
     - Define a variable (``e`` sees the previous ``x``) / assign a variable
   * - ``Declare{x}``
     - ``Def{x, none}``, or nothing when ``x`` is already a variable of the place where ``Def{x, e}`` defines ``x``
       (at the top level, a global variable defined by the file being loaded). The value is ``none``
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
     - Try ``f``, then ``g`` if no pattern of ``f`` matches. ``Fusion{none, g}`` is ``g``
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

A parameter pattern of ``Lambda`` is one of:

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
     - ``_add`` ``_sub`` ``_mul`` ``_div`` ``_mod``
   * - Comparison
     - ``_lt`` ``_le`` ``_gt`` ``_ge`` ``_eq`` ``_ne``
   * - M-expressions
     - ``head`` ``arity`` ``arg`` ``mexpr`` ``push`` ``typeof``
   * - Strings
     - ``strlen`` ``ord`` ``substr`` ``_strcat`` ``intern`` ``symname``
   * - Arrays
     - ``array`` ``array_get`` ``array_set`` ``array_len`` ``hash``
   * - Plain arrays
     - ``u8_array`` ``u16_array`` ``i32_array`` ``plain_get`` ``plain_set`` ``plain_len`` ``plain_kind`` ``plain_copy``
   * - Bit operations
     - ``bit_and`` ``bit_or`` ``bit_xor`` ``bit_not`` ``bit_shl`` ``bit_shr`` ``bit_sar``
   * - Files
     - ``read_file`` ``read_bytes`` ``write_file`` ``chmod``
   * - Language
     - ``parse`` ``eval`` ``exit`` ``argv`` ``_set_file``
   * - ``planck/init.pk``
     - ``true`` ``false`` ``none`` ``_not`` ``load`` ``use`` ``table`` ``table_get`` ``table_set``
   * - ``planck/peg.pk``
     - ``peg_parse`` ``_define_rule``
   * - ``planck/syntax.pk``
     - ``parse_mexpr`` ``_to_text`` ``_record_declare`` ``_record_get`` ``peg_for`` ``_index_get`` ``_index_set`` ``subst``

* ``parse_mexpr(s)``: the parser of M-expressions, with the same arguments and result as ``parse``.
* ``_to_text(x)``: ``x`` for a string, the decimal text for an integer, and ``show(x)`` (``planck/lib/pretty.pk``) otherwise,
  so a value other than a string or an integer in ``{e}`` needs ``use("pretty")``.
  A string with embedded values is joined by ``_strcat`` from the left, and an empty piece is left out.
* ``_record_declare(name, fields)``: set the field names of the head ``name`` (``fields`` is ``Tuple{f, ...}``). ``_record_get(t, name)``: the field.
* ``peg_for(xs, f)``: call ``f`` with each element of the list or vector ``xs``, and return ``none``.
* ``symname(sym)``: the name of the symbol as a string (the inverse of ``intern``).
* ``strlen(s)``: the number of characters. ``ord(s, i)``: the code of the character at ``i``, or ``-1`` when ``i`` is out of range.
  ``s[i]``: the character at ``i`` as a string of length 1.
* ``substr(s, i, j)``: the characters from ``i`` to ``j - 1``. ``_strcat(a, b)``: ``a`` followed by ``b``.
* ``intern(s)``: the symbol named ``s``.
* ``push(Head{a, ...}, x)``: ``Head{a, ..., x}``.
* ``array(n)``: a new array of ``n`` elements, all ``none``. ``array_get(a, i)``, ``array_set(a, i, x)`` (returns ``x``) and ``array_len(a)``.
  An index outside ``0`` to ``n - 1`` is a runtime error.
* ``hash(x)``: a non-negative integer. Values equal by ``_eq`` have the same hash.
* ``table()``: a new hash table ``Table{a}``, where ``a`` is made of arrays. ``table_get(t, key, default)``, ``table_set(t, key, value)``.
  Keys are compared by ``==``.
* ``u8_array(n)``, ``u16_array(n)``, ``i32_array(n)``: a plain array of ``n`` elements, an array of untagged integers, all 0.
  The element is 0 to 255, 0 to 65535 or an integer. ``a[i]`` and ``a[i] = x`` (``plain_get(a, i)``, ``plain_set(a, i, x)``) read and write an element;
  a value that does not fit the type and an index outside ``0`` to ``n - 1`` are runtime errors.
  ``plain_len(a)``: ``n``. ``plain_kind(a)``: ``'u8``, ``'u16`` or ``'i32``.
  ``plain_copy(dst, di, src, si, n)`` copies ``n`` elements; the types must be the same and the ranges may overlap in one array.
* ``bit_and(a, b)``, ``bit_or(a, b)``, ``bit_xor(a, b)``, ``bit_not(a)``: bit by bit on 31-bit two's complement integers.
  ``bit_shl(a, k)`` shifts left by ``k`` (0 to 31) bits and keeps 31 bits. ``bit_shr(a, k)`` is the logical shift of ``a`` as a 31-bit unsigned integer.
  ``bit_sar(a, k)`` is the arithmetic shift.
* ``read_file(path)``: the contents of the file, including bytes 0. ``read_bytes(path)``: the same as a ``u8`` array.
  ``write_file(path, x)``: write all bytes of the string or ``u8`` array ``x`` to the file, which is created or emptied. ``chmod(path, mode)``.
* ``argv()``: the array of the command-line strings of the program, as ``argv`` of C ``main``: the first is the program file and the rest are its arguments.
* ``load(path)``: parse and evaluate the file. Its top-level expressions are evaluated with ``path`` as the file being loaded,
  and the program file given as the first argument and ``planck/init.pk`` are the files being loaded while they run.
* ``_set_file(path)``: make ``path`` the file being loaded and return the previous one (``none`` if there is none).
* ``_define_rule(name, e)``: define the rule by a parsing expression (see ``planck/peg.pk``).
* ``use(name)``: ``load("planck/lib/<name>.pk")`` once.

Libraries
---------

The files of ``planck/lib`` are written in the syntax above and are loaded by ``use("name")``.
Each file uses the files it needs. Lists are ``Cons{x, xs}`` and ``Nil{}``, and the code of a character is the integer that ``ord`` returns.

.. list-table::
   :header-rows: 1

   * - Name
     - Functions
   * - ``base``
     - ``panic(msg)`` prints ``msg`` and stops with status 1, ``assert(c, msg)``
   * - ``list``
     - ``length`` ``reverse`` ``append`` ``concat`` ``map`` ``filter`` ``concat_map`` ``each`` ``foldl`` ``foldr`` ``sum``
       ``nth`` ``last`` ``take`` ``drop`` ``find`` ``any`` ``all`` ``member`` ``index_of`` ``zip`` ``range`` ``list_to_array`` ``sort``
   * - ``vector``
     - ``vec_new`` ``vec_push`` ``vec_pop`` ``vec_get`` ``vec_set`` ``vec_len`` ``vec_to_list`` ``vec_from_list``
   * - ``string``
     - ``is_digit`` ``is_upper`` ``is_lower`` ``is_alpha`` ``is_alnum`` ``is_space`` ``is_ident_start`` ``is_ident_char``
       ``chr`` ``char_at`` ``starts_with`` ``ends_with`` ``index_of_str`` ``str_repeat`` ``str_cmp`` ``str_less`` ``trim``
       ``int_to_str`` ``str_to_int`` ``split`` ``join`` ``strbuf_new`` ``strbuf_add`` ``strbuf_str``
   * - ``u64``
     - ``u64`` ``u64_limbs`` ``u64_hex`` ``u64_to_hex`` ``u64_to_int`` ``u64_add`` ``u64_sub`` ``u64_neg`` ``u64_and`` ``u64_or`` ``u64_xor`` ``u64_not``
       ``u64_shl`` ``u64_shr`` ``u64_sar`` ``u64_eq`` ``u64_ucmp`` ``u64_scmp`` ``u64_ult`` ``u64_slt``
       ``u64_write_le`` ``u64_write_be`` ``u64_read_le`` ``u64_bytes_le``
   * - ``getopt``
     - ``getopt``
   * - ``table``
     - ``table_len`` ``table_has`` ``table_keys`` ``table_values`` ``table_items``
       ``set_new`` ``set_add`` ``set_has`` ``set_len`` ``set_to_list`` ``set_from_list``
   * - ``symbol``
     - ``gensym`` ``gensym_reset``
   * - ``error``
     - ``line_col`` ``line_text`` ``loc_at`` ``show_loc`` ``error_at`` ``catch_error`` ``show_error``
   * - ``pretty``
     - ``quote_str`` ``show`` ``pretty`` ``pprint``
   * - ``unparse``
     - ``unparse`` ``unparse_as`` ``unparse_try`` ``unparse_custom``

List functions:

* ``map(f, xs)``, ``filter(p, xs)`` and ``each(f, xs)`` call ``f`` from the first element to the last.
* ``foldl(f, acc, xs)`` computes ``f(f(acc, x1), x2)``; ``foldr(f, init, xs)`` computes ``f(x1, f(x2, init))``.
* ``nth(xs, i)`` counts from 0. ``take(xs, n)`` and ``drop(xs, n)`` split ``xs`` after ``n`` elements.
* ``find(p, xs)`` is the first element with ``p(x)`` or ``none``. ``index_of(x, xs)`` is the position or ``-1``.
* ``zip(xs, ys)`` is ``[Tuple{x, y}, ...]`` as long as the shorter list. ``range(a, b)`` is ``[a, ..., b - 1]``.
* ``sort(less, xs)`` is a stable merge sort by ``less(a, b)``, e.g. ``sort((a, b) -> a < b, xs)``.

A vector is a growable array ``Vector{a}``: ``vec_push(v, x)`` appends and returns ``x``, ``vec_pop(v)`` removes and returns the last element.
``vec_get`` and ``vec_set`` stop the program for an index outside the elements.

String functions:

* ``chr(c)`` is the one-character string of a code from 32 to 126, 9, 10 or 13. ``char_at(s, i)`` is ``""`` outside ``s``.
* ``index_of_str(s, part, from)`` is the first position at or after ``from`` where ``part`` occurs, or ``-1``.
* ``str_cmp(a, b)`` is ``-1``, ``0`` or ``1`` by the character codes. ``trim(s)`` removes blanks at both ends.
* ``str_to_int(s)`` reads an optional ``-`` and decimal digits, otherwise ``none``.
* ``split(s, sep)`` is the list of the parts between the occurrences of ``sep``, which must not be empty.
  ``join(sep, xs)`` is the inverse.
* A string buffer collects pieces: ``strbuf_add(b, s)`` and ``strbuf_str(b)``.
  The pieces are joined pairwise, so a long string is built without copying the same bytes over and over.

``table_keys``, ``table_values`` and ``table_items`` (``[Tuple{key, value}, ...]``) list the entries in an unspecified order.
A set is a table whose values are ``true``.

A 64-bit value (``use("u64")``) is a ``u16`` array of four 16-bit limbs, the lowest first, which is not changed after it is made.
``u64(n)`` is the value of the integer ``n`` (sign-extended), ``u64_limbs(l3, l2, l1, l0)`` and ``u64_hex("0x...")`` (up to 16 digits) make one,
and ``u64_to_hex(a)`` is ``0x`` and 16 digits. ``u64_to_int(a)`` stops the program unless the signed value fits in 31 bits.
``u64_add``, ``u64_sub`` and ``u64_neg`` are modulo 2 to the 64th power. ``u64_shl``, ``u64_shr`` and ``u64_sar`` take 0 to 63 bits.
``u64_ucmp`` and ``u64_scmp`` are ``-1``, ``0`` or ``1`` as unsigned and as signed, and ``u64_ult`` and ``u64_slt`` are the tests.
``u64_write_le(buf, off, a)`` stores the 8 bytes of ``a`` into the ``u8`` array ``buf`` lowest first (``u64_write_be``: highest first),
``u64_read_le(buf, off)`` reads them, and ``u64_bytes_le(a)`` is a ``u8`` array of them.

Command-line options (``use("getopt")``) are read in the rules of GNU ``getopt_long``. An option is ``Opt{short, long, takes}``.
``getopt(opts, args, from)`` reads the array ``args`` from the index ``from`` and is ``Parsed{table, rest}`` or an error value.
``table`` maps the name of each given option (``long``, or ``short`` when ``long`` is ``""``) to its value, ``true`` for an option without a value,
and ``rest`` is the array of the other strings. ``-a``, ``-abc``, ``-o value``, ``-ovalue``, ``--opt``, ``--opt value`` and ``--opt=value`` are read,
and every string after ``--`` is in ``rest``. ``Unknown{name}`` is an option not in ``opts``, ``Missing{name}`` an option whose value is missing,
and ``NoValue{name}`` is ``--opt=value`` for an option without a value.

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

``unparse(t)`` is a text that parses to the tree ``t`` by the rule ``expr``, made from the grammar in ``peg_sources``.
``unparse_as(rule, t)`` does so for any rule and stops the program when there is no text, and ``unparse_try`` gives ``none`` instead.

* An alternative ``Act{Seq{e1, ...}, Lambda{Tuple{Tuple{l1, ...}}, body}}`` can be inverted when ``body`` is a quasi-quotation
  whose ``!x`` are labels, ``Quote{c}``, a label, ``Tuple{labels}`` or ``mexpr(h, args)``. The tree is matched against the quasi-quotation,
  the labels get the subtrees, and the elements are printed in order. Texts are printed as they are, and a rule by ``unparse_as(rule, subtree)``.
  A label ``_`` is allowed for an element that always matches the same text, such as ``"+"`` or ``Same{}``.
  An element is a text, a rule, or ``Delimited``, ``Delimited1``, ``Many`` or ``Option`` of a rule.
* Among the alternatives that match the tree, the one with the most nodes in the quasi-quotation that are not ``!x`` is used.
* An alternative of a stronger rule where a weaker rule is wanted is put in the first ``"(" e ")"`` of the grammar, whose action is any.
  The alternative that is a rule name only links a rule to the rules it can be, and so decides which alternative needs parentheses.
  An alternative that ends with a looser rule (``while (c) e`` ends with ``expr``) also needs them unless a rule at least as loose is wanted.
* ``unparse_custom(owner, spec, tail, f)`` adds an alternative of the rule ``owner``: ``f(t)`` is the text of ``t`` or ``none``,
  ``spec`` ranks it against the quasi-quotations and ``tail`` is the last rule of the text or ``none``.
  ``unparse`` prints ``Do{...}`` as ``{ }`` with one expression to a line indented by 4 spaces, and ``If`` as ``if (c) a else b``, in this way.
* The text of a tree not written by the grammar is the call of the constructor, ``Head{a, ...}``.
  A symbol that is not an identifier, and a value that is not an integer, a string, a symbol or an M-expression, has no text.
