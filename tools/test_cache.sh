#!/bin/sh
# Tests of the cache that use keeps in .planck-cache.
# usage: sh tools/test_cache.sh     (at the top of the repository, after make planck0)
# The tests run in a copy of planck/ in a directory tmp-cache-test, so the repository is not changed.
PLANCK0=$PWD/planck0
T=tmp-cache-test
failed=0

fail() {
    echo "FAILED: $1"
    failed=$((failed + 1))
}

# expect NAME EXPECTED ACTUAL
expect() {
    if [ "$2" = "$3" ]; then
        echo "ok: $1"
    else
        fail "$1: expected '$2' but got '$3'"
    fi
}

# run PROGRAM: the output of the program, run in $T
run() {
    (cd $T && $PLANCK0 "$1" 2>&1)
}

# files newer than the marker in the cache, as a count
rewritten() {
    find $T/.planck-cache -type f -newer $T/marker 2>/dev/null | wc -l | tr -d ' '
}

rm -rf $T
mkdir $T
cp -r planck $T/planck

# a library that extends the grammar, a library that uses the extension, and a program
cat > $T/planck/lib/cache_ext.pk <<'EOT'
primary ::= 'unless c:expr a:block 'else b:block { `If{!c, !b, !a} } | ...
EOT
cat > $T/planck/lib/cache_use.pk <<'EOT'
use("cache_ext")
cache_val := unless (false) { 1 } else { 2 }
EOT
cat > $T/prog.pk <<'EOT'
use("cache_use")
println(cache_val)
EOT

# the first run writes the cache, the second takes everything from it
expect "first run" "1" "$(run prog.pk)"
touch $T/marker
sleep 1
expect "second run" "1" "$(run prog.pk)"
expect "second run writes nothing" "0" "$(rewritten)"

# the same result without the cache
rm -rf $T/.planck-cache
expect "run without cache" "1" "$(run prog.pk)"

# the text of a library changes
cat > $T/planck/lib/cache_use.pk <<'EOT'
use("cache_ext")
cache_val := unless (false) { 10 } else { 20 }
EOT
expect "library changed" "10" "$(run prog.pk)"

# a library that is used changes the grammar, and the text of the library using it does not change
cat > $T/planck/lib/cache_ext.pk <<'EOT'
primary ::= 'unless c:expr a:block 'else b:block { `If{!c, !a, !b} } | ...
EOT
expect "dependency changed" "20" "$(run prog.pk)"
expect "dependency changed, again" "20" "$(run prog.pk)"

# the grammar is extended by the program before the library is used
cat > $T/planck/lib/cache_ops.pk <<'EOT'
cache_ops_val := 3 ++ 4
EOT
cat > $T/prog_mul.pk <<'EOT'
add_expr ::= a:add_expr "++" b:mul_expr { `Apply{_mul, Tuple{!a, !b}} } | ...
use("cache_ops")
println(cache_ops_val)
EOT
cat > $T/prog_add.pk <<'EOT'
add_expr ::= a:add_expr "++" b:mul_expr { `Apply{_add, Tuple{!a, !b}} } | ...
use("cache_ops")
println(cache_ops_val)
EOT
expect "grammar of the program, first" "12" "$(run prog_mul.pk)"
expect "grammar of the program, other" "7" "$(run prog_add.pk)"
expect "grammar of the program, first again" "12" "$(run prog_mul.pk)"
expect "grammar of the program, other again" "7" "$(run prog_add.pk)"

# a cache file that is broken
break_and_run() {
    # $1: name, $2: command that breaks every cache file in $T/.planck-cache
    rm -rf $T/.planck-cache
    run prog.pk > /dev/null
    for f in $T/.planck-cache/*; do
        sh -c "$2" "$f"
    done
    expect "$1" "20" "$(run prog.pk)"
    expect "$1, after repair" "20" "$(run prog.pk)"
}
break_and_run "cache file truncated" 'head -c 100 "$0" > "$0.t" && mv "$0.t" "$0"'
break_and_run "cache file empty" ': > "$0"'
break_and_run "cache file with a byte changed" 'sed "s/Cache/Cachf/" "$0" > "$0.t" && mv "$0.t" "$0"'
break_and_run "cache file with garbage" 'echo garbage > "$0"'
break_and_run "cache file of 64 digits only" 'printf "0000000000000000000000000000000000000000000000000000000000000000\n" > "$0"'

# a cache directory that cannot be used
rm -rf $T/.planck-cache
: > $T/.planck-cache
expect "cache directory is a file" "20" "$(run prog.pk)"
rm -f $T/.planck-cache

# runs at the same time
rm -rf $T/.planck-cache
for i in 1 2 3 4 5 6 7 8; do
    (cd $T && $PLANCK0 prog.pk > out$i.txt 2>&1) &
done
wait
for i in 1 2 3 4 5 6 7 8; do
    expect "parallel run $i" "20" "$(cat $T/out$i.txt)"
done
touch $T/marker
sleep 1
expect "run after parallel runs" "20" "$(run prog.pk)"
expect "run after parallel runs writes nothing" "0" "$(rewritten)"

# a value without text in an evaluated expression: nothing is cached after it, and the program runs
rm -rf $T/.planck-cache
cat > $T/prog_fn.pk <<'EOT'
g := (x) -> x + 1
println(eval(mexpr('Quote, Tuple{g}))(1))
println(_eval_state())
use("cache_use")
println(cache_val)
EOT
expect "state of a value without text" "2
none
20" "$(run prog_fn.pk)"
expect "nothing cached after it" "0" "$(ls $T/.planck-cache 2>/dev/null | wc -l | tr -d ' ')"

rm -rf $T
if [ $failed -ne 0 ]; then
    echo "$failed tests failed"
    exit 1
fi
echo "cache tests passed"
