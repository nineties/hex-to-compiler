#!/usr/bin/python3
"""src/ の本文の断片に、共通の枠と例（examples/*.pk と GCP で取った *.out）をはめて docs/ に書く。
本文中の書き方:
  <!--ex NAME-->     examples/NAME.pk と examples/NAME.out を「コード＋出力」で入れる
  <!--src NAME-->    examples/NAME.pk だけを入れる
  <!--out NAME-->    examples/NAME.out だけを入れる
  <!--text NAME-->   examples/NAME.txt を planck 以外のコードとして入れる
  <!--bits ... -->   32 ビットの語のビット図（HTML の表）を入れる。中身は1行が1語で「ラベル | 欄の表示/幅 | ...」、幅の合計は 32 でなければ生成が失敗する
  <!--diagram-->     ステージの図（インライン SVG、幅の広い版と狭い版）を入れる
  <!--file F A-->    ソース F の抜粋を入れる。A は行頭の語句で、その行から始まる項目（行頭の行なら次の行頭の行の前の空行まで）。
                     <!--file F A .. B--> は A の行から B の行の直前まで、<!--file F A ..--> は A の行から終わりまで、<!--file F--> は全体
  [[F A]]            ソース F の A の項目へのリンク（[[F A .. B]] も同じ）。A・B はちょうど1行に合わなければ生成が失敗する
                     （(fun f の ( は書かない。fun f と書く）
planck のコードは色付きの HTML にして入れる。色付きの HTML は planck の highlight(planck/lib/highlight.pk)が作り、
hl/<コードの sha1 の先頭12桁>.html に置く(highlight.sh が GCP で作る)。build.py --texts DIR は、色付けが要るコードを DIR/<同じ名前>.pk に書き出す。
1行目は <!--title タイトル--> にする。
src/ja/ が日本語、src/en/ が英語。日本語は docs/ja/ に、英語は docs/en/ に書く。
docs/index.html は言語を選ぶ入口、docs/tutorial/ などの URL には docs/ja/ への移動だけの頁を書く。
"""
import hashlib, html, re, sys
from pathlib import Path

T = Path(__file__).resolve().parent
W = T.parent.parent
SRC, EX, OUT, HL = T / "src", T / "examples", W / "docs", T / "hl"
REPO = "https://github.com/nineties/hex-to-compiler"

SECTIONS = {
    "ja": [
        ("tutorial", "チュートリアル", [
            ("index", "前提と実行"),
            ("1", "1. 式・束縛・関数"),
            ("2", "2. 値と M式"),
            ("3", "3. パターンと融合"),
            ("4", "4. ライブラリ"),
            ("5", "5. 代数的エフェクト"),
            ("6", "6. クオートと評価器"),
            ("7", "7. 文法の拡張"),
            ("8", "8. レコード・局所関数・脱出"),
            ("9", "9. 変数の範囲"),
            ("10", "10. 添字と置き換え"),
        ]),
        ("reference", "リファレンス", [
            ("index", "目次"),
            ("syntax", "構文"),
            ("values", "値・パターン・M式"),
            ("grammar", "文法の宣言"),
            ("builtins", "組み込み関数"),
            ("library", "ライブラリ"),
            ("vasm", "vasm"),
            ("psys", "psys"),
        ]),
        ("implementation", "処理系の実装", [
            ("index", "構成"),
            ("bootstrap", "機械語から planck0 まで"),
            ("planck0", "planck0"),
            ("init", "init.pk と M式"),
            ("peg", "packrat パーサ"),
            ("syntax", "構文の定義"),
            ("vasm", "vasm の検査器"),
            ("planckvm", "planckvm: 実行モデルと値"),
            ("planckvm-isa", "planckvm の命令"),
            ("planckvm-internals", "planckvm の内部仕様"),
        ]),
    ],
    "en": [
        ("tutorial", "Tutorial", [
            ("index", "Prerequisites and running"),
            ("1", "1. Expressions, bindings, functions"),
            ("2", "2. Values and M-expressions"),
            ("3", "3. Patterns and fusion"),
            ("4", "4. Libraries"),
            ("5", "5. Algebraic effects"),
            ("6", "6. Quotation and the evaluator"),
            ("7", "7. Extending the grammar"),
            ("8", "8. Records, local functions, escapes"),
            ("9", "9. Scope of variables"),
            ("10", "10. Indexing and rewriting"),
        ]),
        ("reference", "Reference", [
            ("index", "Contents"),
            ("syntax", "Syntax"),
            ("values", "Values, patterns, M-expressions"),
            ("grammar", "Grammar declarations"),
            ("builtins", "Builtin functions"),
            ("library", "Libraries"),
            ("vasm", "vasm"),
            ("psys", "psys"),
        ]),
        ("implementation", "Implementation", [
            ("index", "Overview"),
            ("bootstrap", "From machine code to planck0"),
            ("planck0", "planck0"),
            ("init", "init.pk and M-expressions"),
            ("peg", "The packrat parser"),
            ("syntax", "Defining the syntax"),
            ("vasm", "The vasm checker"),
            ("planckvm", "planckvm: execution model and values"),
            ("planckvm-isa", "planckvm instructions"),
            ("planckvm-internals", "planckvm internals"),
        ]),
    ],
}

SITE = "hex-to-compiler project"
UI = {
    "ja": dict(sub="自分自身をブートストラップする処理系", overview="概要", out="出力", prev="前のページ", next="次のページ", toc="の目次", switch="English", lang="ja"),
    "en": dict(sub="A compiler that pulls itself up by its bootstraps.", overview="Overview", out="Output", prev="Previous", next="Next", toc=" contents", switch="日本語", lang="en"),
}

LANG = "ja"

# ステージの図。ノード: 名前, 説明 ja, 説明 en, 書かれている言語の key
D_NODES = {
    "xxd":    ("pforth.xxd", "16進ダンプ", "Hex dump", "mc"),
    "pforth": ("pforth", "1文字の語だけの Forth", "Forth with one-letter words", "mc"),
    "plisp":  ("plisp", "Forth の上の Lisp", "A Lisp on top of Forth", "fs"),
    "asm":    ("asm・sasm", "アセンブラと C 風の言語", "Assembler and C-like language", "lisp"),
    "plisp2": ("plisp2", "sasm で書き直した Lisp", "plisp rewritten in sasm", "sasm"),
    "planck0": ("planck0", "planck の最初の処理系", "The first planck implementation", "sasm"),
    "planck": ("planck", "M式の上に載せた言語", "A language on M-expressions", "pk"),
}
D_LANG = {
    "mc": ("機械語", "machine code"),
    "fs": ("Forth", "Forth"),
    "lisp": ("plisp", "plisp"),
    "sasm": ("sasm", "sasm"),
    "pk": ("M式・planck", "M-expr, planck"),
}
# 辺: 元, 先, ラベル（空）
D_EDGES = [
    ("xxd", "pforth", ""),
    ("pforth", "plisp", ""),
    ("plisp", "asm", ""),
    ("asm", "plisp2", ""),
    ("plisp2", "planck0", ""),
    ("planck0", "planck", ""),
]
D_STAGES = [
    (["xxd", "pforth", "plisp"], ("ステージ1　機械語 → Forth → Lisp", "Stage 1: machine code to Lisp")),
    (["asm", "plisp2"], ("ステージ2　機械語を出す道具", "Stage 2: machine code tools")),
    (["planck0", "planck"], ("ステージ3　言語処理系", "Stage 3: the language")),
]


def _tw(t, size):
    return sum(size * (1.0 if ord(c) > 0x2E80 else 0.58) for c in t)


def _svg_node(lang, key, x, y, w, h):
    name, dja, den, lk = D_NODES[key]
    desc = dja if lang == "ja" else den
    lt = D_LANG[lk][0 if lang == "ja" else 1]
    cw = _tw(lt, 12) + 16
    return (f'<g class="dg-n dg-{lk}"><rect x="{x}" y="{y}" width="{w}" height="{h}" rx="8"/>'
            f'<text class="dg-name" x="{x + 12}" y="{y + 25}">{html.escape(name if lang == "ja" else name.replace("・", " / "))}</text>'
            f'<rect class="dg-chip" x="{x + w - cw - 10:.0f}" y="{y + 9}" width="{cw:.0f}" height="20" rx="10"/>'
            f'<text class="dg-chip-t" x="{x + w - cw / 2 - 10:.0f}" y="{y + 23.5}" text-anchor="middle">{html.escape(lt)}</text>'
            f'<text class="dg-desc" x="{x + 12}" y="{y + 47}">{html.escape(desc)}</text></g>')


def _svg_edge(lang, a, b, pos, kind):
    ax, ay, aw, ah = pos[a]
    bx, by, bw, bh = pos[b]
    e = [e for e in D_EDGES if e[0] == a and e[1] == b][0]
    t = e[2]
    lines = (t[lang] if isinstance(t, dict) else t).split("\n")
    mk = f"url(#dg-ah-{kind})"
    out = ""
    if ax == bx:
        x = ax + 26
        y1, y2 = ay + ah, by
        gy1, gy2 = y1, y2
        if (a, b) in (("plisp", "asm"), ("plisp2", "planck0")):
            # 狭い図: ステージの境目を渡る矢印は、前の枠の下から次の枠の上まで引く
            gy1, gy2 = y1 + 12, y2 - 38
            y2 = gy2 + 3
        out += f'<path class="dg-a" d="M{x} {y1 + 3} V{y2 - 5}" marker-end="{mk}"/>'
        for i, l in enumerate(lines):
            ty = (gy1 + gy2) / 2 + (i - (len(lines) - 1) / 2) * 13 + 4
            out += f'<text class="dg-lab" x="{x + 12}" y="{ty:.0f}">{html.escape(l)}</text>'
    else:
        y = ay + ah / 2
        x1, x2 = ax + aw, bx
        out += f'<path class="dg-a" d="M{x1 + 3} {y} H{x2 - 5}" marker-end="{mk}"/>'
        for i, l in enumerate(lines):
            ty = y + 16 + i * 13
            out += f'<text class="dg-lab" x="{(x1 + x2) / 2:.0f}" y="{ty:.0f}" text-anchor="middle">{html.escape(l)}</text>'
    return out


def _svg(lang, kind):
    ix = 0 if lang == "ja" else 1
    nh = 62
    pos = {}
    panels = []
    if kind == "wide":
        nw, colgap, pad = 238, 126, 12
        step = nw + 2 * pad + colgap
        cols = [8 + pad, 8 + pad + step, 8 + pad + 2 * step]
        W = cols[2] + nw + pad + 8
        rowy = lambda r: 44 + r * (nh + 46)
        rows = [[0, 1, 2], [2, 3], [3, 4]]
        for (keys, title), c, rs in zip(D_STAGES, cols, rows):
            for k, r in zip(keys, rs):
                pos[k] = (c, rowy(r), nw, nh)
            top = rowy(rs[0]) - 38
            bot = rowy(rs[-1]) + nh + pad
            panels.append((c - pad, top, nw + 2 * pad, bot - top, title[ix]))
        H = rowy(4) + nh + pad + 8
    else:
        W, nw, pad = 360, 296, 12
        y = 44
        for keys, title in D_STAGES:
            top = y - 38
            for k in keys:
                pos[k] = (32, y, nw, nh)
                y += nh + 42
            y -= 42
            bot = y + pad
            panels.append((8, top, W - 16, bot - top, title[ix]))
            y = bot + 68
        H = y - 68 + 8
    label = "ブートストラップのステージの図" if lang == "ja" else "Diagram of the bootstrap stages"
    s = [f'<svg class="dg dg-{kind}" viewBox="0 0 {W:.0f} {H:.0f}" role="img" aria-label="{label}" xmlns="http://www.w3.org/2000/svg">']
    s.append(f'<defs><marker id="dg-ah-{kind}" markerWidth="8" markerHeight="8" refX="6" refY="4" orient="auto"><path d="M0 0 L8 4 L0 8 z" class="dg-ahp"/></marker></defs>')
    for x, y, w, h, t in panels:
        s.append(f'<rect class="dg-panel" x="{x}" y="{y}" width="{w}" height="{h:.0f}" rx="12"/>')
        s.append(f'<text class="dg-title" x="{x + 14}" y="{y + 24}">{html.escape(t)}</text>')
    for a, b, _ in D_EDGES:
        s.append(_svg_edge(lang, a, b, pos, kind))
    for k, (x, y, w, h) in pos.items():
        s.append(_svg_node(lang, k, x, y, w, h))
    s.append("</svg>")
    return "\n".join(s)


def diagram(lang):
    return '<div class="dg-wrap">\n' + _svg(lang, "wide") + "\n" + _svg(lang, "narrow") + "\n</div>"


def bits(spec):
    """ビット図。1行が1語。ラベルは語の上に、欄は幅に比例した colspan の表のセルに書く。"""
    h = ['<table class="bits">', "<colgroup>" + "<col>" * 32 + "</colgroup>"]
    h.append('<tr class="ruler">' + "".join(
        f"<td>{n // 10 or ''}<br>{n % 10}</td>" for n in range(31, -1, -1)) + "</tr>")
    for line in spec.strip().splitlines():
        label, *fields = [x.strip() for x in line.split("|")]
        total = 0
        cells = []
        for f in fields:
            text, w = f.rsplit("/", 1)
            w = int(w)
            total += w
            cls = ' class="v"' if w <= 2 and len(text) > 1 else ""
            cells.append(f'<td colspan="{w}"{cls}>{html.escape(text)}</td>')
        if total != 32:
            sys.exit(f"bits: width {total} != 32: {line}")
        if label:
            h.append(f'<tr class="lab"><td colspan="32">{html.escape(label)}</td></tr>')
        h.append('<tr class="fld">' + "".join(cells) + "</tr>")
    h.append("</table>")
    return "\n".join(h)


used = set()
used_hl = set()


def ex_file(name, ext):
    p = EX / f"{name}.{ext}"
    if not p.exists():
        sys.exit(f"missing {p}")
    used.add(name)
    return p.read_text(encoding="utf-8")


TEXTS = None


def highlighted(text):
    """planck のコードの色付きの HTML"""
    key = hashlib.sha1(text.encode("utf-8")).hexdigest()[:12]
    if TEXTS is not None:
        (TEXTS / f"{key}.pk").write_text(text, encoding="utf-8")
        return html.escape(text)
    p = HL / f"{key}.html"
    if not p.exists():
        sys.exit(f"missing {p}: run highlight.sh")
    used_hl.add(p.name)
    return p.read_text(encoding="utf-8")


def code_block(name):
    return f'<p class="file">{name}.pk</p>\n<pre class="pk">{highlighted(ex_file(name, "pk").rstrip(chr(10)))}</pre>'


def out_block(name):
    t = ex_file(name, "out").rstrip("\n")
    return f'<p class="label">{UI[LANG]["out"]}</p>\n<pre class="out">{html.escape(t)}</pre>'


def expand(body):
    body = body.replace("<!--diagram-->", diagram(LANG))
    body = re.sub(r"<!--bits\n(.*?)-->", lambda m: bits(m[1]), body, flags=re.S)
    body = re.sub(r"<!--ex (\w+)-->", lambda m: f'<div class="example">\n{code_block(m[1])}\n{out_block(m[1])}\n</div>', body)
    body = re.sub(r"<!--src (\w+)-->", lambda m: f'<div class="example">\n{code_block(m[1])}\n</div>', body)
    body = re.sub(r"<!--out (\w+)-->", lambda m: f'<div class="example">\n{out_block(m[1])}\n</div>', body)
    body = re.sub(r"<!--text (\w+)-->", lambda m: f'<pre>{html.escape(ex_file(m[1], "txt").rstrip(chr(10)))}</pre>', body)
    body = re.sub(r"<!--file ([\w./-]+)((?:(?!-->).)*)-->", excerpt, body)
    body = re.sub(r"\[\[([\w./-]+) ((?:(?!\]\]).)+)\]\]", line_link, body)
    body = body.replace("@REPO@", REPO)
    return body


def _word(c):
    return c.isalnum() or c == "_"


def _find(ls, anchor, ref):
    """anchor で始まる行(行頭の空白と、anchor が ( で始まらなければ行頭の ( を除く)の番号。ちょうど1行でなければ失敗する"""
    hits = []
    for i, l in enumerate(ls):
        s = l.lstrip()
        if not s.startswith(anchor) and s.startswith("(") and not anchor.startswith("("):
            s = s[1:]
        if s.startswith(anchor) and not (_word(anchor[-1]) and _word(s[len(anchor):len(anchor) + 1])):
            hits.append(i)
    if len(hits) != 1:
        sys.exit(f"{ref}: the anchor matches {len(hits)} lines (it must match one)")
    return hits[0]


def _indent(l):
    return len(l) - len(l.lstrip())


def _trim(ls, end):
    while end > 0 and not ls[end - 1].strip():
        end -= 1
    return end


def _item_end(ls, i):
    """行 i から始まる項目の最後の行の番号(1始まり)。行頭の行は、次の行頭の行の前の空行までを1項目とする。
    字下げのある行は、同じ字下げの次の行の前まで(閉じの ) } ] だけの行は含める)"""
    n, s = len(ls), _indent(ls[i])
    j = i + 1
    if s > 0:
        while j < n and (not ls[j].strip() or _indent(ls[j]) > s):
            j += 1
        if j < n and ls[j].lstrip()[0] in ")}]":
            j += 1
        return _trim(ls, j)
    while j < n:
        if not ls[j].strip():
            k = j
            while k < n and not ls[k].strip():
                k += 1
            if k >= n or _indent(ls[k]) == 0:
                break
            j = k
        else:
            j += 1
    return _trim(ls, j)


def span(f, spec, ref):
    """抜粋の範囲(行 a, 行 b)。spec は「A」(A の行から始まる項目)、「A .. B」(A の行から B の行の直前まで)、
    「A ..」(A の行からファイルの終わりまで)、空(ファイル全体)"""
    ls = (W / f).read_text(encoding="utf-8").splitlines()
    spec = spec.strip()
    if not spec:
        return 1, _trim(ls, len(ls)), ls
    a_s, dots, b_s = spec.partition(" ..")
    i = _find(ls, a_s.strip(), ref)
    if i > 0 and _indent(ls[i]) > 0 and ls[i - 1].strip() in ("Def{", "Set{"):
        i -= 1  # 名前が次の行にある Def{ ... の定義は、Def{ の行から
    if not dots:
        return i + 1, _item_end(ls, i), ls
    if not b_s.strip():
        return i + 1, _trim(ls, len(ls)), ls
    k = _find(ls, b_s.strip(), ref)
    if k <= i:
        sys.exit(f"{ref}: the end anchor is before the start anchor")
    return i + 1, _trim(ls, k), ls


def _label(f, spec):
    name = spec.partition(" ..")[0].strip()
    name = re.sub(r"^(?:[;#\\\"(]\s*|=+\s*)+|(?:\s*[\"=):])+$", "", name)
    return html.escape(f"{f}: {name}" if name else f)


def excerpt(m):
    f, spec = m[1], m[2]
    a, b, ls = span(f, spec, m[0])
    text = "\n".join(ls[a - 1:b])
    if f.endswith(".pk"):
        cls, body = ' class="pk"', highlighted(text)
    else:
        cls, body = "", html.escape(text)
    anchor = f"#L{a}-L{b}" if b > a else f"#L{a}"
    return (f'<div class="example">\n<p class="file"><a href="{REPO}/blob/main/{f}{anchor}">{_label(f, spec)}</a></p>\n'
            f'<pre{cls}>{body}</pre>\n</div>')


def line_link(m):
    f, spec = m[1], m[2]
    a, b, _ = span(f, spec, m[0])
    anchor = f"#L{a}-L{b}" if b > a else f"#L{a}"
    return f'<a href="{REPO}/blob/main/{f}{anchor}"><code>{_label(f, spec)}</code></a>'


def href(frm_sec, to_sec, to_page):
    up = "../" if frm_sec else ""
    if to_sec is None:
        return up + "index.html"
    return f"{up}{to_sec}/{to_page}.html"


def nav(cur_sec):
    u = UI[LANG]
    items = [f'<a{' class="here"' if cur_sec is None else ""} href="{href(cur_sec, None, None)}">{u["overview"]}</a>']
    for sec, label, pages in SECTIONS[LANG]:
        cls = ' class="here"' if sec == cur_sec else ""
        items.append(f'<a{cls} href="{href(cur_sec, sec, "index")}">{label}</a>')
    return "\n".join(items)


def other_lang_href(cur_sec, cur_page):
    """今の頁の、もう一方の言語の頁への相対パス"""
    path = f"{cur_sec}/{cur_page}.html" if cur_sec else "index.html"
    other_code = "en" if LANG == "ja" else "ja"
    return ("../" if cur_sec else "") + f"../{other_code}/" + path


def page(cur_sec, cur_page, title, body, toc_pages, sec_label):
    u = UI[LANG]
    toc = ""
    pager = ""
    if cur_sec:
        lis = []
        for p, t in toc_pages:
            cls = ' class="here"' if p == cur_page else ""
            lis.append(f'<li><a{cls} href="{p}.html">{t}</a></li>')
        toc = f'''<nav class="toc" aria-label="{sec_label}{u["toc"]}"><details open><summary>{sec_label}{u["toc"]}</summary>
<h2>{sec_label}</h2>
<ol>
{chr(10).join(lis)}
</ol></details></nav>'''
        idx = [p for p, _ in toc_pages].index(cur_page)
        prev = toc_pages[idx - 1] if idx > 0 else None
        nxt = toc_pages[idx + 1] if idx + 1 < len(toc_pages) else None
        a = f'<a class="prev" href="{prev[0]}.html"><small>{u["prev"]}</small>{prev[1]}</a>' if prev else ""
        b = f'<a class="next" href="{nxt[0]}.html"><small>{u["next"]}</small>{nxt[1]}</a>' if nxt else ""
        pager = f'<nav class="pager">{a}{b}</nav>'
    depth = 1 + (1 if cur_sec else 0)
    up = "../" * depth
    other = other_lang_href(cur_sec, cur_page)
    other_code = "en" if LANG == "ja" else "ja"
    full_title = f"{title} | {SITE}" if title != SITE else title
    body = body.replace("@UP@", up)
    return f'''<!DOCTYPE html>
<html lang="{u["lang"]}">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{full_title}</title>
<link rel="alternate" hreflang="{other_code}" href="{other}">
<link rel="stylesheet" href="{up}assets/style.css">
</head>
<body>
<header class="site-header"><div class="inner">
<a class="logo" href="{href(cur_sec, None, None)}"><span class="brand"><b>hex-to-compiler <i>project</i></b><small>{u["sub"]}</small></span></a>
<nav class="site-nav">
{nav(cur_sec)}
<a href="{REPO}">GitHub</a>
<a class="lang-switch" lang="{other_code}" hreflang="{other_code}" href="{other}">{u["switch"]}</a>
</nav>
</div></header>
<div class="layout{"" if cur_sec else " single"}">
{toc}
<main>
{body}
{pager}
</main>
</div>
<footer><div class="inner"><a href="{REPO}">github.com/nineties/hex-to-compiler</a></div></footer>
<script src="{up}assets/page.js"></script>
</body>
</html>
'''


def read_src(path):
    s = path.read_text(encoding="utf-8")
    m = re.match(r"<!--title (.*?)-->\n", s)
    if not m:
        sys.exit(f"no title in {path}")
    return m[1], s[m.end():]


def build_lang(lang):
    global LANG
    LANG = lang
    src = SRC / lang
    out = OUT / lang
    n = 0
    title, body = read_src(src / "index.html")
    out.mkdir(parents=True, exist_ok=True)
    (out / "index.html").write_text(page(None, None, title, expand(body), [], ""), encoding="utf-8")
    n += 1
    for sec, label, pages in SECTIONS[lang]:
        (out / sec).mkdir(parents=True, exist_ok=True)
        for p, t in pages:
            title, body = read_src(src / sec / f"{p}.html")
            (out / sec / f"{p}.html").write_text(page(sec, p, title, expand(body), pages, label), encoding="utf-8")
            n += 1
    return n


def entrance():
    """docs/index.html: 言語を選ぶ入口"""
    return f'''<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{SITE}</title>
<link rel="stylesheet" href="assets/style.css">
</head>
<body>
<div class="layout single">
<main>
<h1>{SITE}</h1>
<p><a href="ja/index.html">日本語</a></p>
<p><a href="en/index.html">English</a></p>
</main>
</div>
</body>
</html>
'''


def moved(to):
    """移動先を示す頁"""
    return f'''<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{SITE}</title>
<meta http-equiv="refresh" content="0; url={to}">
<link rel="stylesheet" href="../assets/style.css">
</head>
<body>
<div class="layout single">
<main>
<p><a href="{to}">{to}</a></p>
</main>
</div>
</body>
</html>
'''


def build_entrance():
    (OUT / "index.html").write_text(entrance(), encoding="utf-8")
    for sec, _, pages in SECTIONS["ja"]:
        (OUT / sec).mkdir(parents=True, exist_ok=True)
        for p, _ in pages:
            (OUT / sec / f"{p}.html").write_text(moved(f"../ja/{sec}/{p}.html"), encoding="utf-8")


def main():
    global TEXTS
    if len(sys.argv) == 3 and sys.argv[1] == "--texts":
        TEXTS = Path(sys.argv[2])
        TEXTS.mkdir(parents=True, exist_ok=True)
    build_entrance()
    counts = {lang: build_lang(lang) for lang in ("ja", "en") if (SRC / lang).exists()}
    unused = sorted(p.stem for p in EX.glob("*.pk") if p.stem not in used)
    stale = sorted(p.name for p in HL.glob("*.html") if p.name not in used_hl) if TEXTS is None else []
    if TEXTS is not None:
        (TEXTS / "index.txt").write_text("".join(f"{q.stem}\n" for q in sorted(TEXTS.glob("*.pk"))), encoding="utf-8")
    print("built pages:", counts, "examples used:", len(used), "unused:", unused, "highlighted:", len(used_hl), "stale:", stale)


main()
