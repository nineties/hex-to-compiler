// planck-run.js: runs the planck code of a page in the browser.
// A block of the tutorial with data-run (the mode "run" or "repl") and data-src (the code) gets a button that runs it, and the page of the
// playground (id="playground") has an editor. The code runs in planck-worker.js on the PlanckVM of vm.wasm.
(function () {
  "use strict";

  var base = document.currentScript.src.replace(/[^\/]*$/, "");
  var ja = document.documentElement.lang === "ja";
  var text = {
    run: ja ? "実行" : "Run",
    running: ja ? "実行中" : "Running",
    stopped: ja ? "時間がかかりすぎたので止めました" : "Stopped because it took too long",
    failed: ja ? "実行できませんでした" : "Could not run",
    out: ja ? "出力" : "Output"
  };
  var LIMIT_MS = 30000;

  // run(mode, code, done) calls done with {status, stdout, stderr}. A run is in its own worker, which is stopped after LIMIT_MS.
  function run(mode, code, done) {
    var w;
    try {
      w = new Worker(base + "planck-worker.js");
    } catch (e) {
      done({ status: -1, stdout: "", stderr: String(e) });
      return;
    }
    var timer = setTimeout(function () {
      w.terminate();
      done({ status: -2, stdout: "", stderr: text.stopped });
    }, LIMIT_MS);
    w.onmessage = function (ev) {
      clearTimeout(timer);
      w.terminate();
      done(ev.data);
    };
    w.onerror = function (ev) {
      clearTimeout(timer);
      w.terminate();
      done({ status: -1, stdout: "", stderr: ev.message || text.failed });
    };
    w.postMessage({ mode: mode, text: code });
  }

  function show(pre, r) {
    pre.textContent = r.stdout + (r.status !== 0 ? r.stderr : "");
  }

  function attach(block) {
    var mode = block.getAttribute("data-run");
    var code = block.getAttribute("data-src");
    var button = document.createElement("button");
    button.type = "button";
    button.className = "run";
    button.textContent = text.run;
    var label = document.createElement("p");
    label.className = "label";
    label.textContent = text.out;
    var pre = document.createElement("pre");
    pre.className = "out run-out";
    var shown = false;
    button.addEventListener("click", function () {
      button.disabled = true;
      button.textContent = text.running;
      run(mode, code, function (r) {
        if (!shown) {
          block.appendChild(label);
          block.appendChild(pre);
          shown = true;
        }
        show(pre, r);
        button.disabled = false;
        button.textContent = text.run;
      });
    });
    block.appendChild(button);
  }

  function playground(root) {
    var src = root.querySelector("#pg-src");
    var mode = root.querySelector("#pg-mode");
    var button = root.querySelector("#pg-run");
    var pre = root.querySelector("#pg-out");
    button.addEventListener("click", function () {
      button.disabled = true;
      button.textContent = text.running;
      run(mode.value, src.value, function (r) {
        show(pre, r);
        button.disabled = false;
        button.textContent = text.run;
      });
    });
  }

  var blocks = document.querySelectorAll("[data-run]");
  for (var i = 0; i < blocks.length; i++) attach(blocks[i]);
  var pg = document.getElementById("playground");
  if (pg) playground(pg);
})();
