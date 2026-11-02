// 広い画面では目次を開き、狭い画面では畳んでおく。
(function () {
  var d = document.querySelector(".toc details");
  if (d && !window.matchMedia("(min-width: 900px)").matches) d.removeAttribute("open");
})();
