#!/usr/bin/python3
"""src/ の本文の断片に、共通の枠と例（examples/*.pk と GCP で取った *.out）をはめて docs/ に書く。
本文中の書き方:
  <!--ex NAME-->     examples/NAME.pk と examples/NAME.out を「コード＋出力」で入れる
  <!--src NAME-->    examples/NAME.pk だけを入れる
  <!--out NAME-->    examples/NAME.out だけを入れる
1行目は <!--title タイトル--> にする。
"""
import html, re, sys
from pathlib import Path

T = Path(__file__).resolve().parent
W = T.parent.parent
SRC, EX, OUT = T / "src", T / "examples", W / "docs"
REPO = "https://github.com/nineties/hex-to-compiler"

SECTIONS = [
    ("tutorial", "チュートリアル", [
        ("index", "はじめに"),
        ("1", "1. はじめの一歩"),
        ("2", "2. 変数と関数"),
        ("3", "3. 値のかたち"),
        ("4", "4. パターンで分ける"),
        ("5", "5. ライブラリを使う"),
        ("6", "6. レコードと流れの制御"),
        ("7", "7. エフェクト"),
        ("8", "8. 構文を広げる"),
        ("9", "9. プログラムはデータ"),
    ]),
    ("reference", "リファレンス", [
        ("index", "目次"),
        ("syntax", "構文"),
        ("values", "値・パターン・M式"),
        ("grammar", "文法の宣言"),
        ("builtins", "組み込み関数"),
        ("library", "ライブラリ"),
    ]),
    ("implementation", "処理系の実装", [
        ("index", "全体の地図"),
        ("bootstrap", "機械語から planck0 まで"),
        ("planck0", "planck0 の中身"),
        ("init", "init.pk と M式"),
        ("peg", "packrat パーサ"),
        ("syntax", "構文を planck で書く"),
    ]),
]

used = set()


def ex_file(name, ext):
    p = EX / f"{name}.{ext}"
    if not p.exists():
        sys.exit(f"missing {p}")
    used.add(name)
    return p.read_text(encoding="utf-8")


def code_block(name):
    return f'<p class="file">{name}.pk</p>\n<pre class="pk">{html.escape(ex_file(name, "pk").rstrip(chr(10)))}</pre>'


def out_block(name):
    t = ex_file(name, "out").rstrip("\n")
    return f'<p class="label">出力</p>\n<pre class="out">{html.escape(t)}</pre>'


def expand(body):
    body = re.sub(r"<!--ex (\w+)-->", lambda m: f'<div class="example">\n{code_block(m[1])}\n{out_block(m[1])}\n</div>', body)
    body = re.sub(r"<!--src (\w+)-->", lambda m: f'<div class="example">\n{code_block(m[1])}\n</div>', body)
    body = re.sub(r"<!--out (\w+)-->", lambda m: f'<div class="example">\n{out_block(m[1])}\n</div>', body)
    body = re.sub(r"<!--file ([\w./-]+) (\d+)-(\d+)-->", excerpt, body)
    body = re.sub(r"\[\[([\w./-]+)#(\d+)(?:-(\d+))?\]\]", line_link, body)
    body = body.replace("@REPO@", REPO)
    return body


def excerpt(m):
    f, a, b = m[1], int(m[2]), int(m[3])
    ls = (W / f).read_text(encoding="utf-8").splitlines()
    if b > len(ls):
        sys.exit(f"excerpt out of range: {m[0]}")
    cls = ' class="pk"' if f.endswith(".pk") else ""
    text = "\n".join(ls[a - 1:b])
    return (f'<div class="example">\n<p class="file"><a href="{REPO}/blob/main/{f}#L{a}-L{b}">{f}:{a}–{b}</a></p>\n'
            f'<pre{cls}>{html.escape(text)}</pre>\n</div>')


def line_link(m):
    f, a, b = m[1], m[2], m[3]
    src = W / f
    n = len(src.read_text(encoding="utf-8").splitlines())
    if int(b or a) > n:
        sys.exit(f"line out of range: {m[0]} ({n} lines)")
    anchor = f"#L{a}-L{b}" if b else f"#L{a}"
    label = f"{f}:{a}–{b}" if b else f"{f}:{a}"
    return f'<a href="{REPO}/blob/main/{f}{anchor}"><code>{label}</code></a>'


def href(frm_sec, to_sec, to_page):
    up = "../" if frm_sec else ""
    if to_sec is None:
        return up + "index.html"
    return f"{up}{to_sec}/{to_page}.html"


def nav(cur_sec):
    items = []
    for sec, label, pages in SECTIONS:
        cls = ' class="here"' if sec == cur_sec else ""
        items.append(f'<a{cls} href="{href(cur_sec, sec, "index")}">{label}</a>')
    return "\n".join(items)


def page(cur_sec, cur_page, title, body, toc_pages, sec_label):
    toc = ""
    pager = ""
    if cur_sec:
        lis = []
        for p, t in toc_pages:
            cls = ' class="here"' if p == cur_page else ""
            lis.append(f'<li><a{cls} href="{p}.html">{t}</a></li>')
        toc = f'''<nav class="toc" aria-label="{sec_label}の目次"><details open><summary>{sec_label}の目次</summary>
<h2>{sec_label}</h2>
<ol>
{chr(10).join(lis)}
</ol></details></nav>'''
        idx = [p for p, _ in toc_pages].index(cur_page)
        prev = toc_pages[idx - 1] if idx > 0 else None
        nxt = toc_pages[idx + 1] if idx + 1 < len(toc_pages) else None
        a = f'<a class="prev" href="{prev[0]}.html"><small>前のページ</small>{prev[1]}</a>' if prev else ""
        b = f'<a class="next" href="{nxt[0]}.html"><small>次のページ</small>{nxt[1]}</a>' if nxt else ""
        pager = f'<nav class="pager">{a}{b}</nav>'
    up = "../" if cur_sec else ""
    full_title = f"{title} | planck" if title != "planck" else "planck"
    return f'''<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{full_title}</title>
<link rel="stylesheet" href="{up}assets/style.css">
</head>
<body>
<header class="site-header"><div class="inner">
<a class="logo" href="{up}index.html">planck<span>.</span></a>
<nav class="site-nav">
{nav(cur_sec)}
<a href="{REPO}">GitHub</a>
</nav>
</div></header>
<div class="layout{"" if cur_sec else " single"}">
{toc}
<main>
{body}
{pager}
</main>
</div>
<footer><div class="inner">planck — <a href="{REPO}">nineties/hex-to-compiler</a></div></footer>
<script src="{up}assets/highlight.js"></script>
</body>
</html>
'''


def read_src(path):
    s = path.read_text(encoding="utf-8")
    m = re.match(r"<!--title (.*?)-->\n", s)
    if not m:
        sys.exit(f"no title in {path}")
    return m[1], s[m.end():]


def main():
    title, body = read_src(SRC / "index.html")
    (OUT / "index.html").write_text(page(None, None, title, expand(body), [], ""), encoding="utf-8")
    for sec, label, pages in SECTIONS:
        (OUT / sec).mkdir(parents=True, exist_ok=True)
        for p, t in pages:
            title, body = read_src(SRC / sec / f"{p}.html")
            (OUT / sec / f"{p}.html").write_text(page(sec, p, title, expand(body), pages, label), encoding="utf-8")
    unused = sorted(p.stem for p in EX.glob("*.pk") if p.stem not in used)
    print("built; examples used:", len(used), "unused:", unused)


main()
