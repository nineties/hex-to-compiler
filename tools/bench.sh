#!/bin/sh
# Benchmarks of the bootstrap stages and of sample programs.
# usage: sh tools/bench.sh [runs]     (run at the top of the repository; default 5 runs)
# Needs only sh, awk, xxd and GNU time (/usr/bin/time -v).
# Output in bench-out/:
#   raw.tsv      name run elapsed_s max_rss_kb exit_status cpu_s load1   (every run)
#   summary.tsv  name median_elapsed_s median_max_rss_kb median_cpu_s median_load1 failed_runs
#   sizes.tsv    name bytes
#   outputs.txt  whether each sample output matches its .out file
#   env.txt      date, kernel, CPU, cores, load, commit
#   summary.md   the tables
N=${1:-5}
OUT=bench-out
rm -rf $OUT
mkdir -p $OUT
RAW=$OUT/raw.tsv
: > $RAW
: > $OUT/outputs.txt
: > $OUT/sizes.tsv
{
  echo "date: $(date '+%Y-%m-%d %H:%M:%S %Z')"
  echo "kernel: $(uname -r) $(uname -m)"
  echo "cpu: $(grep -m1 'model name' /proc/cpuinfo | sed 's/.*: //')"
  echo "cores: $(nproc)"
  echo "load_before: $(cut -d' ' -f1-3 /proc/loadavg)"
  echo "commit: ${GITHUB_SHA:-$(git rev-parse HEAD 2>/dev/null)}"
} > $OUT/env.txt

# bench NAME CMD: run CMD (via sh -c) N times under /usr/bin/time -v
bench() {
  name=$1
  shift
  i=1
  while [ $i -le $N ]; do
    /usr/bin/time -v -o $OUT/t.txt sh -c "$*" > $OUT/last.out 2> $OUT/last.err
    rc=$?
    el=$(awk '/Elapsed/ {n=split($NF,a,":"); s=0; for(k=1;k<=n;k++) s=s*60+a[k]; print s}' $OUT/t.txt)
    kb=$(awk '/Maximum resident/ {print $NF}' $OUT/t.txt)
    cpu=$(awk '/User time/ {u=$NF} /System time/ {s=$NF} END {printf "%.2f", u+s}' $OUT/t.txt)
    ld=$(cut -d' ' -f1 /proc/loadavg)
    printf '%s\t%d\t%s\t%s\t%d\t%s\t%s\n' "$name" $i "$el" "$kb" $rc "$cpu" "$ld" >> $RAW
    i=$((i+1))
  done
  cp $OUT/last.out "$OUT/out-$name.txt"
}
size() { printf '%s\t%s\n' "$1" "$(wc -c < "$2" | tr -d ' ')" >> $OUT/sizes.tsv; }

# stage 1: hex -> forth
bench s1-pforth 'xxd -r -c 8 pforth.xxd > pforth; chmod +x pforth'
size s1-pforth pforth

# stage 1: forth -> lisp (plisp): load plisp.fs and run its test suite
bench s1-plisp './pforth < plisp.fs plisp/test.lisp'
size s1-plisp plisp.fs

# stage 2: plisp2 built by sasm running on plisp
bench s2-plisp2 './pforth < plisp.fs sasm.lisp plisp2.sv tmp-plisp2; chmod +x tmp-plisp2'
mv tmp-plisp2 plisp2
size s2-plisp2 plisp2

# stage 2: asm and sasm on plisp and on plisp2
bench s2-asm-hello-plisp './pforth < plisp.fs asm.lisp asm/examples/hello.s tmp-hello'
bench s2-asm-hello-plisp2 './plisp2 asm.lisp asm/examples/hello.s tmp-hello'
size s2-asm-hello tmp-hello
bench s2-sasm-fib-plisp './pforth < plisp.fs sasm.lisp sasm/examples/fib.sv tmp-fib'
bench s2-sasm-fib-plisp2 './plisp2 sasm.lisp sasm/examples/fib.sv tmp-fib'
size s2-sasm-fib tmp-fib
chmod +x tmp-fib
bench s2-run-sasm-fib './tmp-fib'

# stage 3: planck0 built by sasm running on plisp2
bench s3-planck0 './plisp2 sasm.lisp planck0.sv tmp-planck0; chmod +x tmp-planck0'
mv tmp-planck0 planck0
size s3-planck0 planck0

# planck0 samples (each output is compared with the .out beside it)
for f in fib queens calc deriv nondet generator simplify unparse; do
  bench ex-$f "./planck0 planck/examples/$f.pk"
  if cmp -s "$OUT/out-ex-$f.txt" planck/examples/$f.out; then
    echo "ex-$f: output matches" >> $OUT/outputs.txt
  else
    echo "ex-$f: OUTPUT DIFFERS" >> $OUT/outputs.txt
  fi
done

# libraries
printf 'use("base")\n' > $OUT/lib-base.pk
printf 'use("base")\nuse("list")\n' > $OUT/lib-list.pk
printf 'use("base")\nuse("list")\nuse("string")\n' > $OUT/lib-string.pk
bench lib-base "./planck0 $OUT/lib-base.pk"
bench lib-list "./planck0 $OUT/lib-list.pk"
bench lib-string "./planck0 $OUT/lib-string.pk"

# planck0 compiling a vasm program to an x86 executable
bench vasmc-array './planck0 planck/vasmc.pk planck/vasm/array.vasm tmp-vasm'
size vasmc-array tmp-vasm

# medians (lower middle for an even count)
awk -F'\t' '
 function med(arr,k,c,   i,j,x,a) {
   for (i=1;i<=c;i++) a[i]=arr[k,i]
   for (i=2;i<=c;i++) { x=a[i]; for (j=i-1;j>=1 && a[j]>x;j--) a[j+1]=a[j]; a[j+1]=x }
   return a[int((c+1)/2)]
 }
 { k=$1; c[k]++; t[k,c[k]]=$3; m[k,c[k]]=$4; u[k,c[k]]=$6; l[k,c[k]]=$7; if ($5 != 0) r[k]++; if (!(k in o)) { o[k]=++no; nm[no]=k } }
 END { for (i=1;i<=no;i++) { k=nm[i]; printf "%s\t%s\t%s\t%s\t%s\t%d\n", k, med(t,k,c[k]), med(m,k,c[k]), med(u,k,c[k]), med(l,k,c[k]), r[k] } }' $RAW > $OUT/summary.tsv
echo "load_after: $(cut -d' ' -f1-3 /proc/loadavg)" >> $OUT/env.txt

# markdown tables
{
  echo "### Environment"
  echo
  echo '```'
  cat $OUT/env.txt
  echo '```'
  echo
  echo "### Medians of $N runs"
  echo
  echo "| step | elapsed (s) | CPU user+sys (s) | max memory (MiB) | output (bytes) | failed runs |"
  echo "|---|---:|---:|---:|---:|---:|"
  awk -F'\t' '
    FILENAME ~ /sizes/ { sz[$1]=$2; next }
    { key=$1; sub(/-(plisp2?)$/, "", key)
      b = ($1 in sz) ? sz[$1] : ((key in sz) ? sz[key] : "")
      printf "| %s | %.2f | %.2f | %.1f | %s | %d |\n", $1, $2, $4, $3/1024, b, $6 }' $OUT/sizes.tsv $OUT/summary.tsv
  echo
  echo "### Sample outputs"
  echo
  sed 's/^/- /' $OUT/outputs.txt
} > $OUT/summary.md

rm -f tmp-hello tmp-fib tmp-vasm $OUT/last.out $OUT/last.err $OUT/t.txt
