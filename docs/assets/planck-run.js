// planck-run.js: runs the planck code of a page in the browser.
// A block of the tutorial with data-run (the mode "run" or "repl") and data-src (the code) gets a button that runs it, and the page of the
// playground (id="playground") is a REPL. The code runs in planck-worker.js on the PlanckVM of vm.wasm.
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
    pre.textContent = (r.stdout + (r.status !== 0 ? r.stderr : "")).replace(/\n$/, "");
  }

  function attach(block) {
    var mode = block.getAttribute("data-run");
    var code = block.getAttribute("data-src");
    var button = document.createElement("button");
    button.type = "button";
    button.className = "run";
    button.textContent = text.run;
    // The output is added by the first run and replaced by the later ones.
    var label = document.createElement("p");
    label.className = "label";
    label.textContent = text.out;
    var pre = document.createElement("pre");
    pre.className = "out run-out";
    button.addEventListener("click", function () {
      button.disabled = true;
      button.textContent = text.running;
      run(mode, code, function (r) {
        if (label) {
          block.appendChild(label);
          block.appendChild(pre);
          label = null;
        }
        show(pre, r);
        button.disabled = false;
        button.textContent = text.run;
      });
    });
    block.appendChild(button);
  }

  // The Playground is a REPL. The VM runs a program in a fresh worker, so a line is evaluated together with the lines accepted before it, and only
  // the part of the session that is new is shown. A final expression that only prints its echo tells that the evaluation was not stopped by an error.
  function playground(root) {
    var log = root.querySelector("#pg-log");
    var input = root.querySelector("#pg-in");
    var MARK = "\"playground-mark\"";
    var history = "";
    var shown = "";
    var busy = false;

    function add(session) {
      var lines = session.replace(/\n$/, "").split("\n");
      for (var i = 0; i < lines.length; i++) {
        var l = lines[i];
        var span = document.createElement("span");
        if (l.indexOf("> ") === 0 || l.indexOf("... ") === 0) {
          var k = l.indexOf(" ") + 1;
          var p = document.createElement("span");
          p.className = "prompt";
          p.textContent = l.slice(0, k);
          span.appendChild(p);
          span.appendChild(document.createTextNode(l.slice(k)));
        } else {
          span.className = "res";
          span.textContent = l;
        }
        log.appendChild(span);
        log.appendChild(document.createTextNode("\n"));
      }
      window.scrollTo(0, document.body.scrollHeight);
    }

    function fit() {
      input.style.height = "auto";
      input.style.height = input.scrollHeight + "px";
    }

    function submit() {
      var line = input.value.replace(/\s+$/, "");
      if (line === "" || busy) return;
      busy = true;
      input.disabled = true;
      run("repl", history + line + "\n" + MARK + "\n", function (r) {
        var full = r.stdout;
        var ok = full.slice(0, shown.length) === shown && full.indexOf("> " + MARK + "\n") >= 0;
        if (ok) {
          var cut = full.lastIndexOf("> " + MARK + "\n");
          var fresh = full.slice(shown.length, cut);
          add(fresh);
          shown = full.slice(0, cut);
          history += line + "\n";
        } else if (r.status !== 0 && r.stdout === "") {
          add(r.stderr);
        } else {
          // an error: show what the new line wrote, and do not keep the line
          var from = full.slice(0, shown.length) === shown ? shown.length : 0;
          add(full.slice(from));
        }
        input.value = "";
        input.disabled = false;
        busy = false;
        fit();
        input.focus();
      });
    }

    input.addEventListener("keydown", function (ev) {
      if (ev.key === "Enter" && !ev.shiftKey && !ev.isComposing) {
        ev.preventDefault();
        submit();
      }
    });
    input.addEventListener("input", fit);
    input.focus();
  }

  var blocks = document.querySelectorAll("[data-run]");
  for (var i = 0; i < blocks.length; i++) attach(blocks[i]);
  var pg = document.getElementById("playground");
  if (pg) playground(pg);
})();
