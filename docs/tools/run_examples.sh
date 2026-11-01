#!/bin/sh
# 例を GCP の planck0 で動かし、出力を examples/*.out に書く
# 使い方: GCP_HOST=user@host run_examples.sh [名前 ...]（名前がなければ全部）
# GCP_HOST の ~/tmp/planck-docs/hex に、planck0 をビルドしたこのリポジトリがあること
set -e
H=${GCP_HOST:?GCP_HOST=user@host を指定する}
cd "$(dirname "$0")/examples"
if [ $# -gt 0 ]; then L=$(for n in "$@"; do echo $n.pk; done); else L=$(ls *.pk); fi
R=$(mktemp -d)
tar cf - $L | ssh $H 'rm -rf ~/tmp/planck-docs/ex && mkdir -p ~/tmp/planck-docs/ex && cd ~/tmp/planck-docs/ex && tar xf - && cd ~/tmp/planck-docs/hex && for f in ../ex/*.pk; do ./planck0 $f > ${f%.pk}.out 2> ${f%.pk}.err; echo $? > ${f%.pk}.rc; done; cd ../ex && tar cf - *.out *.err *.rc' | tar xf - -C "$R"
for f in $L; do
  b=${f%.pk}
  cp "$R/$b.out" "$b.out"
  rc=$(cat "$R/$b.rc")
  if [ "$rc" != 0 ] || [ -s "$R/$b.err" ]; then echo "NG $b rc=$rc: $(head -c 300 "$R/$b.err")"; fi
done
echo done
