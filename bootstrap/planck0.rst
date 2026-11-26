planck0
=======

``planck0`` runs a program written as m-expressions. It reads each top-level
expression of a file, compiles it and evaluates it. This page describes the
main interpreter loop, the special forms and the built-in functions.

A program is a sequence of m-expressions. An m-expression is a head and
arguments written ``Head{arg, ...}``. Integers, strings and symbols are the
atoms. ``Tuple{a, b}`` is an m-expression whose head is ``Tuple``.

The main interpreter loop
-------------------------

``planck0 [file]`` runs ``planck/init.pk`` in the current directory, then
``file``. ``planck0 --version`` and ``planck0 --info`` print information about
the processor and exit. ``os_argv()`` returns the command-line arguments after
the program name, so its first element is ``file``.

For each file the loop does the following until the end of the text.

1. Parse the next m-expression by calling the global variable ``parse`` with
   ``Quote{text}``, which gives ``Tuple{expression, rest}``, where ``rest`` is
   the text after the expression. The variable is looked up for every
   expression.
2. Stop at ``Eof{}``. A syntax error prints the file name and the error to
   stderr and exits with status 1.
3. Evaluate the expression by calling the global variable ``eval`` with
   ``Quote{expression}``. The variable is looked up for every expression.

The initial values of ``parse`` and ``eval`` are the built-in functions. A
program that assigns another function to ``parse`` or ``eval`` with
``Set{eval, ...}`` or ``Def{eval, ...}`` makes that function read or evaluate
the following top-level expressions. Code compiled before a ``Def`` keeps calling the old function;
``Set`` replaces the function for the code compiled before it as well.

An error that no program can recover from (a type error, an index out of
range, an unhandled effect) prints ``name: message`` to stderr and exits with
status 1. The functions that depend on the outside world return ``Ok{value}``
or ``Err{error}`` instead, as described below.

Special forms
-------------

A special form is an m-expression whose head ``planck0`` compiles itself. Any
other head makes an m-expression whose arguments are evaluated first, so
``Foo{1, 2}`` is a new ``Foo`` m-expression. ``Tuple{...}`` is the same.

``Apply{f, Tuple{a, b}}``
   Calls the function ``f`` with the arguments. The second argument must be a
   ``Tuple``. A call in tail position does not grow the stack, except in a
   top-level expression.

``Def{x, e}``
   Evaluates ``e`` and defines the variable ``x`` with the value. ``e`` refers
   to the previous variable of the same name, so ``Def{x, Apply{int_add,
   Tuple{x, 1}}}`` is allowed after ``Def{x, 1}``.

``DefFun{f, Lambda{...}}``
   Defines ``f`` as ``Fusion{f, Lambda{...}}``. Repeating it adds a clause to
   the function.

``Set{x, e}``
   Assigns the value of ``e`` to the existing variable ``x``.

``Do{e1, e2, ...}``
   Evaluates the expressions in order and gives the last value. ``Do{}`` gives
   ``none``.

``If{c, a, b}``, ``If{c, a}``
   Evaluates ``a`` if ``c`` is true and ``b`` otherwise. ``If{c, a}`` gives
   ``none`` when ``c`` is false. Any other number of arguments is malformed.

``While{c, body}``
   Repeats ``body`` while ``c`` is true. The value is ``none``.

``Lambda{Tuple{p1, p2, ...}, body}``
   Makes a function. A parameter is a symbol or a pattern. A name may appear
   only once in the parameters and patterns of a ``Lambda``; a second
   occurrence stops compilation with ``not implemented: duplicate parameter
   name``. The type of a ``_TypeOf{q, t}`` pattern is evaluated where the
   function is made.

``Fusion{f, g}``
   Gives a function that tries ``f`` and then ``g``, so it works as one
   function with several clauses. If ``f`` is ``none``, the value is ``g``.

``Quote{e}``
   Gives ``e`` unevaluated.

``QuasiQuote{e}``, ``UnQuote{e}``
   ``QuasiQuote`` is like ``Quote`` except that each ``UnQuote{x}`` inside it
   is replaced by the value of ``x``. ``UnQuote`` outside of ``QuasiQuote``
   is a compile error.

``Handle{body, handler}``, ``Perform{v}``
   ``Handle`` evaluates ``body``. When ``body`` evaluates ``Perform{v}``, the
   nearest enclosing handler is called with ``v`` and the continuation of the
   ``Perform``. The value of the handler is the value of the ``Handle``;
   calling the continuation with a value resumes ``body`` with that value as
   the value of the ``Perform``. A ``Perform`` without a handler is a runtime
   error.

Example (``planck0 example.pk``)::

   Def{x, 1}
   Def{x, Apply{int_add, Tuple{x, 1}}}
   Apply{os_put, Tuple{Apply{int_to_str, Tuple{x, 10}}}}
   Apply{os_put, Tuple{Apply{int_to_str, Tuple{Handle{Apply{int_add, Tuple{1, Perform{5}}},
       Lambda{Tuple{v, k}, Apply{k, Tuple{v}}}}, 10}}}}

prints ``26``: ``x`` is 2, and the handler resumes ``1 + Perform{5}`` with 5.

Built-in functions
------------------

The built-in functions are global variables. A name has a prefix for what it
works on: ``int_``, ``str_``, ``sym_``, ``array_``, ``parray_``, ``bit_``,
``mexpr_``, ``type``, ``os_`` and ``gc_``. A failure other than the ``Ok`` and
``Err`` results below prints the function name and a message and exits with
status 1. An index is a non-negative integer.

Integers
~~~~~~~~

Integers have arbitrary precision.

``int_add``, ``int_sub``, ``int_mul`` (``a, b``)
   Sum, difference and product.

``int_div(a, b)``, ``int_quo(a, b)``, ``int_mod(a, b)``
   ``int_div`` rounds the quotient toward zero. ``int_quo`` rounds it toward
   negative infinity, and ``int_mod`` is the remainder of ``int_quo``, so the
   result has the sign of ``b``. ``int_div(-7, 2)`` is ``-3``, ``int_quo(-7, 2)``
   is ``-4`` and ``int_mod(-7, 2)`` is ``1``. A zero divisor is an error.

``int_lt``, ``int_le``, ``int_gt``, ``int_ge`` (``a, b``)
   Comparisons. They give ``true`` or ``false``.

``int_to_str(n, base)``
   The digits of ``n`` in ``base``, from 2 to 36, with ``-`` for a negative
   number and without a prefix.

``str_to_int(s)``
   The integer of ``s``. The prefixes ``0x``, ``0o`` and ``0b`` select the
   base; the default is 10. An invalid text is an error.

``bit_and``, ``bit_or``, ``bit_xor``, ``bit_not``, ``bit_shl``, ``bit_sar``
   Bit operations as on two's complement integers. ``bit_shl(1, 40)`` is
   ``1099511627776``. A negative shift count is an error, and ``bit_shl``
   stops with an error that names it when the result does not fit in memory.

Values and types
~~~~~~~~~~~~~~~~

``equal(a, b)``, ``not_equal(a, b)``
   Compare by content. Integers, strings, m-expressions, arrays, parrays and
   the values of a type made by ``type_new`` are equal when their contents
   are equal. Any other pair is equal only when identical.

``identical(a, b)``
   True when ``a`` and ``b`` are the same object. Integers are identical when
   they have the same value.

``hash(x)``
   A non-negative integer that is the same for values that are equal by
   ``equal``.

``type(v)``
   The type of ``v``: ``Int``, ``String``, ``Symbol``, ``Expr``, ``Function``,
   ``Array``, ``PArray`` or a type made by ``type_new``. ``Type`` is the type
   of types. These are global variables that hold the types.

``type_name(t)``
   The name of the type ``t`` as a symbol.

``type_new(name)``
   Makes a type and gives ``Tuple{t, wrap, unwrap}``. ``wrap(v)`` makes a
   value of type ``t`` that holds ``v``, and ``unwrap(x)`` gives ``v`` back.
   ``unwrap`` stops with an error for a value of another type::

      Def{t, Apply{type_new, Tuple{Quote{Meters}}}}
      Def{wrap, Apply{mexpr_arg, Tuple{t, 1}}}
      Def{m, Apply{wrap, Tuple{5}}}

   ``type(m)`` is the type named ``Meters``.

Strings and symbols
~~~~~~~~~~~~~~~~~~~

A string is immutable and is a sequence of bytes. Indexes and lengths count
bytes. ``planck0`` supports only ASCII characters: a byte of 128 or more in a
source file, a string literal, ``os_read_file``, the output of ``os_run``, a
name from ``os_list_dir`` or an argument stops with ``not implemented`` when
the string is made, and so does ``str_chr`` with a code above 127. Every
string is ASCII, so the other string functions do not check.

A string has no terminating NULL character; its length is in its header.
``os_`` functions that pass a string to the operating system make a copy
that ends with NULL just before the call, so a string made by ``str_sub`` can
be passed as it is. A symbol name is stored with a terminating NULL and
``sym_intern`` copies the name only when the symbol is new.

``str_len(s)``, ``str_ord(s, i)``, ``str_chr(c)``
   The length, the character code at ``i`` and the one-character string of a
   code. ``str_ord`` and ``str_chr`` stop with an error when ``i`` is out of
   range and when ``c`` is above 127.

``str_sub(s, i, j)``, ``str_cat(a, b)``
   The characters from ``i`` up to ``j`` and the concatenation.
   ``str_sub`` always shares the original string and neither copies nor
   scans it.

``str_join(strings)``
   The concatenation of all the strings in ``strings``, made at once, where
   ``strings`` is an array or a ``Tuple{...}``. An empty one gives the empty
   string. An element that is not a string is an error.

``str_find(s, part, from)``
   The position of the first occurrence of ``part`` in ``s`` at or after
   ``from``, or -1 when there is none. An empty ``part`` gives ``from``. A
   ``from`` outside 0 to ``str_len(s)`` is an error.

``str_cmp(a, b)``
   Compares the bytes of ``a`` and ``b`` in order. The result is -1 when ``a``
   is smaller, 0 when they are the same and 1 when ``a`` is larger. A string
   that is a prefix of another is the smaller.

``sym_intern(s)``, ``sym_name(sym)``
   The symbol of a string and the name of a symbol. A string with a NULL
   character is an error.

M-expressions
~~~~~~~~~~~~~

``mexpr_head(m)``, ``mexpr_arity(m)``, ``mexpr_arg(m, i)``
   The head, the number of arguments and the argument at ``i``.

``mexpr_new(head, args)``
   A new m-expression with the head and the arguments, where ``args`` is a
   ``Tuple{...}`` or an array. The elements of an array are the arguments.

``mexpr_args(m)``
   The arguments of ``m`` copied to a new array, the inverse of
   ``mexpr_new(head, array)``.

``parse(s)``
   Parses the first m-expression in ``s`` and gives ``Tuple{expression, rest}``.
   The expression is ``Eof{}`` at the end of the text and ``SyntaxError{...}``
   when the text is invalid.

``eval(e)``
   Compiles and evaluates the m-expression ``e``. The main loop calls it
   through the global variable ``eval``.

Arrays
~~~~~~

``array_new(n)``
   A mutable array of ``n`` elements, all ``none``.

``array_get(a, i)``, ``array_set(a, i, v)``, ``array_len(a)``
   Element access. ``array_set`` gives ``none``.

``array_copy(dst, di, src, si, n)``
   Copies ``n`` elements. The ranges may overlap.

A parray holds unboxed elements of one kind: ``parray_new_u8(n)``,
``parray_new_u16(n)`` and ``parray_new_i32(n)`` make a zero-filled parray.
``parray_len``, ``parray_get``, ``parray_set`` and ``parray_copy`` are
``array_len``, ``array_get``, ``array_set`` and ``array_copy`` for parrays,
with ``parray_copy`` requiring the same kind on both sides. ``parray_kind(a)``
is the symbol ``u8``, ``u16`` or ``i32``. ``parray_set`` stops with an error
for a value out of range for the kind.

Operating system
~~~~~~~~~~~~~~~~

``os_put(s)``
   Writes the string to standard output.

``os_argv()``
   The command-line arguments as an array of strings.

``os_exit(n)``
   Exits with status ``n``, from 0 to 255.

``os_time_ns()``
   The current time in nanoseconds since 1970-01-01 00:00:00 UTC, as
   ``time.time_ns()`` of Python gives. The value does not overflow in 2038.

``os_read_file(path)``, ``os_read_bytes(path)``
   ``Ok{contents}`` as a string and as a u8 parray.

``os_write_file(path, s)``, ``os_write_bytes(path, a)``
   Write a string and a u8 parray to a file, which is created or truncated, and
   give ``Ok{none}``.

``os_chmod(path, mode)``
   Sets the permissions, from 0 to ``0o7777``, and gives ``Ok{none}``.

``os_list_dir(path)``
   ``Ok{names}``, an array of the names in the directory in sorted order.

``os_run(path, args, input, dir)``
   Runs the program ``path`` with ``args``, an array or a ``Tuple{...}``, as
   the arguments after
   the program name, ``input`` as standard input, and ``dir`` as the working
   directory unless it is empty. It gives ``Ok{[status, stdout, stderr]}``,
   where ``status`` is the exit status, or -1 when a signal ended the program.

The functions that return ``Ok`` and ``Err`` give ``Err{IoError{stage, path}}``
when the system call named by the symbol ``stage`` fails. A path or a program
name with a NULL character is an error.

Memory
~~~~~~

``gc_allocated_bytes()``
   The total number of bytes allocated since the start.

``gc_count()``
   The number of collections so far.

The global variables ``planck_target``, ``planck_version`` and
``planck_features`` describe the processor; ``planck0 --info`` prints them.
