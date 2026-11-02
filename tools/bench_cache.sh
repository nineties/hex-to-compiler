#!/bin/sh
# usage: sh tools/bench_cache.sh BASE_DIR NEW_DIR
# Times of a vasm example, of loading the libraries of the compiler and of all tests,
# without the cache (BASE_DIR is a build of main), with the cache empty and with the cache filled.
BASE=$1
NEW=$2
N=5
echo "date: $(date)"
echo "cpu: $(grep -m1 'model name' /proc/cpuinfo | sed 's/.*: //') cores: $(nproc)"
echo "base: $(git -C $NEW rev-parse origin/main)  new: $(git -C $NEW rev-parse HEAD)"

# run DIR LABEL PREP COMMAND...: N runs, the median of the elapsed seconds and of the maximum resident set size
run() {
    dir=$1; label=$2; prep=$3; shift 3
    : > /tmp/bench_times
    i=0
    while [ $i -lt $N ]; do
        (cd $dir && sh -c "$prep")
        (cd $dir && /usr/bin/time -f "%e %M" -o /tmp/bench_one "$@" > /dev/null 2>&1)
        cat /tmp/bench_one >> /tmp/bench_times
        i=$((i + 1))
    done
    t=$(awk '{print $1}' /tmp/bench_times | sort -n | awk '{a[NR]=$1} END {print a[int((NR+1)/2)]}')
    m=$(awk '{print $2}' /tmp/bench_times | sort -n | awk '{a[NR]=$1} END {print a[int((NR+1)/2)]}')
    all=$(awk '{printf "%s ", $1}' /tmp/bench_times)
    echo "$label: median ${t} s, max rss ${m} KB (runs: $all)"
}

cat > $BASE/bench_use.pk <<'EOT'
use("vasm_build")
EOT
cp $BASE/bench_use.pk $NEW/bench_use.pk

echo "== vasm example (planck/vasm/array.vasm), $N runs each"
run $BASE "no cache (main)" ":" ./planck0 planck/vasmc.pk planck/vasm/array.vasm out-bench
run $NEW "first run (cache empty)" "rm -rf .planck-cache" ./planck0 planck/vasmc.pk planck/vasm/array.vasm out-bench
(cd $NEW && ./planck0 planck/vasmc.pk planck/vasm/array.vasm out-bench-warm > /dev/null)
run $NEW "second run (cache filled)" ":" ./planck0 planck/vasmc.pk planck/vasm/array.vasm out-bench-warm
cmp $BASE/out-bench $NEW/out-bench-warm && echo "the executables are the same"
(cd $NEW && ./planck0 planck/vasmc.pk planck/vasm/array.vasm out-bench-cold) && cmp $BASE/out-bench $NEW/out-bench-cold && echo "the executable without the cache is the same"

echo "== only use(\"vasm_build\"), $N runs each"
run $BASE "no cache (main)" ":" ./planck0 bench_use.pk
run $NEW "first run (cache empty)" "rm -rf .planck-cache" ./planck0 bench_use.pk
run $NEW "second run (cache filled)" ":" ./planck0 bench_use.pk

echo "== an empty program, $N runs each"
echo "" > $BASE/bench_empty.pk
cp $BASE/bench_empty.pk $NEW/bench_empty.pk
run $BASE "no cache (main)" ":" ./planck0 bench_empty.pk
run $NEW "this branch" ":" ./planck0 bench_empty.pk

echo "== make test-planck0, one run each"
timeit() {
    dir=$1; label=$2; prep=$3
    (cd $dir && sh -c "$prep")
    s=$(date +%s)
    (cd $dir && make test-planck0 > /tmp/bench_test.log 2>&1); rc=$?
    e=$(date +%s)
    echo "$label: $((e - s)) s (exit status $rc)"
}
timeit $BASE "no cache (main)" ":"
timeit $NEW "cache empty at the start" "rm -rf .planck-cache"
timeit $NEW "cache filled" ":"
