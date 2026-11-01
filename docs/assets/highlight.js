// planck のコードに色を付ける。<pre class="pk"> の中身を字句に分けて span で囲む。
(function () {
  var kw = /^(if|else|while|and|or|not|handle|with|perform|match|record|for|in|return|break|continue|fun)$/;
  var re = /(#[^\n]*)|(f?"(?:[^"\\\n]|\\.)*")|(\b0[xob][0-9a-fA-F]+\b|\b\d+\b)|('[A-Za-z_][A-Za-z0-9_]*)|([A-Za-z_][A-Za-z0-9_]*)(?=\{)|([A-Za-z_][A-Za-z0-9_]*)/g;
  function esc(s) { return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"); }
  function paint(src) {
    var out = "", last = 0, m;
    re.lastIndex = 0;
    while ((m = re.exec(src))) {
      out += esc(src.slice(last, m.index));
      var t = m[0], c = null;
      if (m[1]) c = "com";
      else if (m[2]) c = "str";
      else if (m[3]) c = "num";
      else if (m[4]) c = "sym";
      else if (m[5]) c = "head";
      else if (m[6] && kw.test(t)) c = "kw";
      out += c ? '<span class="tok-' + c + '">' + esc(t) + "</span>" : esc(t);
      last = re.lastIndex;
    }
    return out + esc(src.slice(last));
  }
  var blocks = document.querySelectorAll("pre.pk");
  for (var i = 0; i < blocks.length; i++) blocks[i].innerHTML = paint(blocks[i].textContent);
})();
// 広い画面では目次を開き、狭い画面では畳んでおく。
(function () {
  var d = document.querySelector(".toc details");
  if (d && !window.matchMedia("(min-width: 900px)").matches) d.removeAttribute("open");
})();
