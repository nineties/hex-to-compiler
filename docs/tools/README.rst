docs/ の作り直し
================

docs/ の HTML（英語版は docs/en/）は、頁の元 (日本語は ``src/ja/``、英語は ``src/en/``)、例 (``examples/*.pk``)、例の出力 (``examples/*.out``) から ``build.py`` が組む。

例の出力は planck0 を実際に動かして取る。planck0 のビルドは x86_64 の GCP ノードで行う。

1. GCP ノードの ``~/tmp/planck-docs/hex`` にこのリポジトリを置き、``make`` で planck0 を作る。別の場所に置いたときは ``GCP_DIR=tmp/<名前>`` で ``~`` からの位置を渡す。
2. 例を動かして ``examples/*.out`` を更新する。

   ::

       GCP_HOST=user@host docs/tools/run_examples.sh [名前 ...]

   終了コードが 0 でない例や標準エラーに出力がある例は ``NG`` と表示される。

3. HTML を組む。

   ::

       python3 docs/tools/build.py

頁の元の書き方は ``build.py`` の先頭にある。
