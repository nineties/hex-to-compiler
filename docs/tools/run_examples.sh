#!/bin/sh
# 例を GCP の planck0 で動かし、出力を examples/*.out に書く
# 使い方: GCP_HOST=user@host run_examples.sh [名前 ...]（名前がなければ全部）
# GCP_HOST の ~/$GCP_DIR/hex（既定 tmp/planck-docs）に、planck0 をビルドしたこのリポジトリがあること
# NAME.pk はプログラムで、出力が NAME.out になる。NAME.repl は REPL の入力で、repl.pk を前に付けて動かし、
# プロンプトの形の記録（"> 式" の次の行に値）が NAME.out になる
# 名前が e_ で始まる例は止まる例で、標準出力と標準エラー出力を合わせて .out に書く（終了コードが 0 なら NG）
set -e
H=${GCP_HOST:?GCP_HOST=user@host を指定する}
D=${GCP_DIR:-tmp/planck-docs}
T=$(cd "$(dirname "$0")" && pwd)
cd "$T/examples"
if [ $# -gt 0 ]; then L=$(for n in "$@"; do echo $n; done); else L=$(ls *.pk *.repl | sed 's/\.[a-z]*$//' | sort -u); fi
S=$(mktemp -d)
R=$(mktemp -d)
for n in $L; do
  if [ -f $n.repl ]; then cat "$T/repl.pk" $n.repl > "$S/$n.pk"; else cp $n.pk "$S/$n.pk"; fi
done
(cd "$S" && tar cf - $(for n in $L; do echo $n.pk; done)) | ssh $H "rm -rf ~/$D/ex && mkdir -p ~/$D/ex && cd ~/$D/ex && tar xf - && cd ~/$D/hex && "' for f in ../ex/*.pk; do case ${f##*/} in e_*) ./planck0 $f > ${f%.pk}.out 2>&1; echo $? > ${f%.pk}.rc; : > ${f%.pk}.err;; *) ./planck0 $f > ${f%.pk}.out 2> ${f%.pk}.err; echo $? > ${f%.pk}.rc;; esac; done; cd ../ex && tar cf - *.out *.err *.rc' | tar xf - -C "$R"
for b in $L; do
  cp "$R/$b.out" "$b.out"
  rc=$(cat "$R/$b.rc")
  case $b in
    e_*) if [ "$rc" = 0 ]; then echo "NG $b: should stop"; fi ;;
    *) if [ "$rc" != 0 ] || [ -s "$R/$b.err" ]; then echo "NG $b rc=$rc: $(head -c 300 "$R/$b.err")"; fi ;;
  esac
done
rm -rf "$S" "$R"
echo done
