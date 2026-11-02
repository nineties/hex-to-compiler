#!/bin/sh
# 文書の planck のコードを GCP の planck0 で色付けし、hl/*.html に書く
# 使い方: GCP_HOST=user@host highlight.sh
# GCP_HOST の ~/$GCP_DIR/hex（既定 tmp/planck-docs）に、planck0 をビルドしたこのリポジトリがあること
# 色付けは planck/lib/highlight.pk。全部のコードを1回の planck0 で色付けする（コードの中の文法の宣言は highlight が戻す）
set -e
H=${GCP_HOST:?GCP_HOST=user@host を指定する}
D=${GCP_DIR:-tmp/planck-docs}
here=$(cd "$(dirname "$0")" && pwd)
T=$(mktemp -d)
python3 "$here/build.py" --texts "$T/in" > /dev/null
mkdir -p "$here/hl"
tar cf - -C "$T" in | ssh $H "rm -rf ~/$D/hl && mkdir -p ~/$D/hl/out && cd ~/$D/hl && tar xf - && cd ~/$D/hex && ./planck0 docs/tools/highlight.pk ~/$D/hl/in ~/$D/hl/out && cd ~/$D/hl && tar cf - out" | tar xf - -C "$T"
n=0
for f in "$T"/in/*.pk; do
  b=$(basename "$f" .pk)
  if [ ! -s "$T/out/$b.html" ]; then echo "NG $b"; continue; fi
  cp "$T/out/$b.html" "$here/hl/$b.html"
  n=$((n + 1))
done
for f in "$here"/hl/*.html; do
  [ -e "$T/in/$(basename "$f" .html).pk" ] || rm -f "$f"
done
echo "done $n"
