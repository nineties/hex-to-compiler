#!/bin/sh
# tests of the evaluator written in Planck (planck/lib/interp.pk, tools/interp.pk)
# usage: tools/test_interp.sh [--full-syntax]   (run in the top directory, after make planck0)
#
# Each program runs with planck0 and with the evaluator. The output, the error output and the exit status must be the same.
# With --full-syntax the evaluator loads planck/peg.pk and planck/syntax.pk itself and a few small programs run,
# which takes minutes for each program and most of the memory of planck0.
set -e
ROOT=$(pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

MODE=""
if [ "$1" = "--full-syntax" ]; then MODE="--full-syntax"; fi

NATIVE="
planck/tests/core.pk
planck/tests/define.pk
planck/tests/lib.pk
planck/tests/syntax.pk
planck/tests/unparse.pk
planck/tests/unparse_pattern.pk
planck/tests/unparse_file.pk
planck/tests/highlight.pk
planck/tests/backend.pk
planck/tests/vasm.pk
planck/tests/vasm2.pk
$(ls planck/examples/*.pk)
$(ls planck/tests/errors/*.pk)
"

FULL="
planck/examples/fib.pk
planck/examples/state.pk
planck/examples/exception.pk
planck/examples/list.pk
"

fail=0
same() { # same <program> <arguments>
    p=$1; shift
    "$ROOT/planck0" "$p" "$@" > "$T/host.out" 2> "$T/host.err" && hs=0 || hs=$?
    "$ROOT/planck0" tools/interp.pk $MODE "$p" "$@" > "$T/int.out" 2> "$T/int.err" && is=0 || is=$?
    if [ "$hs" = "$is" ] && cmp -s "$T/host.out" "$T/int.out" && cmp -s "$T/host.err" "$T/int.err"; then
        echo "$p: same"
    else
        echo "$p: different (status $hs, $is)"
        diff "$T/host.out" "$T/int.out" | head -5 || true
        diff "$T/host.err" "$T/int.err" | head -5 || true
        fail=1
    fi
}

# the evaluator and the libraries it uses are read from their preprocessed files at each run
./planck0 tools/preprocess.pk interp > /dev/null
if [ -z "$MODE" ]; then
    ./planck0 planck/tests/interp.pk
    for p in $NATIVE; do same "$p"; done
    same planck/tests/builtins.pk one "two words"
else
    for p in $FULL; do same "$p"; done
fi
[ "$fail" = 0 ] || exit 1
