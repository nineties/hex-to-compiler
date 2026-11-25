hex-to-compiler
===============

.. image:: https://github.com/nineties/hex-to-compiler/actions/workflows/runtests.yaml/badge.svg?branch=main&event=push
   :target: https://github.com/nineties/hex-to-compiler/actions/workflows/runtests.yaml
   :alt: run tests

A programming language, Planck, bootstrapped from hand-written machine code.
Each stage is built only with the tools made in the earlier stages: no external
assembler, compiler or runtime is used.

This repository extends `planckforth <https://github.com/nineties/planckforth>`_.

How it is built
---------------

1. ``pforth``: a Forth interpreter written in machine code
   (`bootstrap/pforth.xxd <bootstrap/pforth.xxd>`_).
2. ``plisp``: a Lisp interpreter written in Forth
   (`bootstrap/plisp.fs <bootstrap/plisp.fs>`_), run by ``pforth``.
3. ``asm`` and ``sasm``: an assembler (`bootstrap/asm.lisp <bootstrap/asm.lisp>`_)
   and a C-like structured assembly (`bootstrap/sasm.lisp <bootstrap/sasm.lisp>`_),
   both written in Lisp.
4. ``plisp2``: ``plisp`` rewritten in ``sasm``
   (`bootstrap/plisp2.sv <bootstrap/plisp2.sv>`_). It is much faster than ``plisp``.
5. ``planck0``: the Planck language processor written in ``sasm``
   (`bootstrap/planck0.sv <bootstrap/planck0.sv>`_). It is a static 32-bit i386 ELF
   of about 79KB.

Build
-----

On x86_64 Linux::

   $ git clone https://github.com/nineties/hex-to-compiler.git
   $ cd hex-to-compiler
   $ make

``make`` builds ``pforth``, ``plisp2`` and ``planck0`` in the top directory.

Other targets:

* ``make example``: build the examples under ``bootstrap/``
* ``make test``: test ``plisp``
* ``make test-planck0``: test ``planck0`` with ``planck/init.pk``
* ``make clean``: remove the built files

Use a release binary
--------------------

Download ``planck0`` and ``planck0.sha256`` from the
`Releases <https://github.com/nineties/hex-to-compiler/releases>`_ page::

   $ sha256sum -c planck0.sha256
   $ chmod +x planck0

``planck0`` reads ``planck/init.pk`` from the current directory, so run it in a
directory that has the ``planck`` folder of this repository.

The Planck language
-------------------

The definition of the Planck language (the syntax) and its libraries are in
`nineties/planck <https://github.com/nineties/planck>`_. This repository keeps only
what ``planck0`` needs to start: ``planck/init.pk``, and ``planck/tests/core.pk``
that tests it.

Run a file with ``planck0``::

   $ ./planck0 planck/tests/core.pk
   Tests passed 149/149

Integers are 31 bits and become arbitrary-precision integers on overflow.
There is no floating point. Strings are sequences of bytes (ASCII).

Arguments after the file name go to the program.

Information about the processor is in the global variables planck_target
("i386-linux"), planck_version (a string) and planck_features (an array
of strings, empty in planck0). ./planck0 --version prints planck0 and the version,
and ./planck0 --info prints target:, version: and features:; both
exit without reading a program.

Test
----

::

   $ make test-planck0   # planck/tests/core.pk

GitHub Actions runs ``make test`` and ``make test-planck0`` on every push.

Development Blog
----------------

*Language: Japanese*

* `機械語手書きから言語処理系をブートストラップする <https://qiita.com/9_ties/items/349b2ed65b7cd8a7d580>`_
* `機械語手書きから言語処理系をブートストラップする(2) <https://qiita.com/9_ties/items/d70390d2a7f18b45b4d3>`_
* `機械語手書きから言語処理系をブートストラップする(3) <https://qiita.com/9_ties/items/689071cf51f1a8c15507>`_

License
-------

See the LICENSE file.
