#!/bin/sh
# tests of the preprocessed libraries (planck/preprocess.pk, use)
# usage: tools/test_preprocess.sh   (run in the top directory, after make planck0)
set -e
ROOT=$(pwd)
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
mkdir -p "$T/planck/lib"
for f in init peg syntax use preprocess; do ln -s "$ROOT/planck/$f.pk" "$T/planck/$f.pk"; done

fail() { echo "test_preprocess: $1"; exit 1; }
run() { (cd "$T" && "$ROOT/planck0" "$@"); }
expect() { # expect <name> <expected output> <program>
    got=$(run "$3")
    [ "$got" = "$2" ] || fail "$1: expected '$2', got '$got'"
    echo "$1: ok"
}

cat > "$T/planck/lib/glib.pk" <<'EOT'
v := 5 * 2
EOT
cat > "$T/p1.pk" <<'EOT'
use("glib")
println(v)
EOT
# a program which changes the grammar before it uses the library
cat > "$T/p2.pk" <<'EOT'
mul_expr ::= a:mul_expr "*" b:unary { `(!a - !b) } | ...
use("glib")
println(v)
EOT

expect "without a preprocessed file" 10 p1.pk
run planck/preprocess.pk glib
[ -f "$T/planck/lib/glib.pkm" ] || fail "preprocess did not make glib.pkm"
expect "with a preprocessed file" 10 p1.pk

# the preprocessed file is used: its expression is read instead of the text of the library
cp "$T/planck/lib/glib.pkm" "$T/glib.good"
sed 's/Apply{_mul, Tuple{5, 2}}/Apply{_add, Tuple{5, 2}}/' "$T/glib.good" > "$T/planck/lib/glib.pkm"
expect "the preprocessed expressions are evaluated" 7 p1.pk

# a stale file, a broken file and a grammar which differs are not used
cp "$T/glib.good" "$T/planck/lib/glib.pkm"
expect "grammar declared before use" 3 p2.pk
head -c 100 "$T/glib.good" > "$T/planck/lib/glib.pkm"
expect "truncated file" 10 p1.pk
sed '$d' "$T/glib.good" > "$T/planck/lib/glib.pkm"
expect "file without the end" 10 p1.pk
printf 'Preprocessed{1' > "$T/planck/lib/glib.pkm"
expect "unreadable file" 10 p1.pk
printf 'garbage ]]]' > "$T/planck/lib/glib.pkm"
expect "garbage" 10 p1.pk
: > "$T/planck/lib/glib.pkm"
expect "empty file" 10 p1.pk
cp "$T/glib.good" "$T/planck/lib/glib.pkm"
echo 'v := 5 * 3' > "$T/planck/lib/glib.pk"
expect "library changed after preprocessing" 15 p1.pk
echo 'v := 5 * 2' > "$T/planck/lib/glib.pk"
expect "library restored" 10 p1.pk

# libraries which use each other: the library which declares a rule is parsed again when the grammar differs
cat > "$T/planck/lib/glib2.pk" <<'EOT'
use("glib")
w := v * 3
EOT
cat > "$T/p3.pk" <<'EOT'
use("glib2")
println(w)
EOT
expect "libraries which use each other" 30 p3.pk
run planck/preprocess.pk glib2
[ -f "$T/planck/lib/glib2.pkm" ] || fail "preprocess did not make glib2.pkm"
expect "libraries preprocessed" 30 p3.pk

# the executable made with the preprocessed libraries is the same as without them
if [ -f "$ROOT/planck/lib/vasm_build.pkm" ]; then
    for ex in hello fib; do
        "$ROOT/planck0" planck/vasmc.pk "planck/vasm/$ex.vasm" "$T/$ex.pre" || fail "$ex: vasmc with preprocessed libraries"
        mkdir -p "$T/off"
        mv planck/lib/*.pkm "$T/off/"
        "$ROOT/planck0" planck/vasmc.pk "planck/vasm/$ex.vasm" "$T/$ex.plain" || fail "$ex: vasmc without preprocessed libraries"
        mv "$T"/off/*.pkm planck/lib/
        cmp "$T/$ex.pre" "$T/$ex.plain" || fail "$ex: the executables differ"
        echo "$ex: same executable"
    done
fi
echo "test_preprocess: all passed"
