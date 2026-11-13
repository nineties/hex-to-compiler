// planck-worker.js: runs a program in the PlanckVM of vm.wasm, in a Web Worker so that a long run does not stop the page.
// The page sends {mode, text}, where mode is "run" or "repl" (see docs/tools/playground_run.pk), and receives {status, stdout, stderr}.
importScripts("planck-vm.js");

var HEAP_BYTES = 134217728;
var loaded = null;

function load() {
  if (loaded === null) {
    loaded = Promise.all([
      fetch("vm.wasm").then(function (r) { return r.arrayBuffer(); }).then(WebAssembly.compile),
      fetch("planck.img").then(function (r) { return r.arrayBuffer(); })
    ]);
  }
  return loaded;
}

onmessage = function (ev) {
  load().then(function (v) {
    var files = { "planck.img": new Uint8Array(v[1]) };
    var r = PlanckVM.run(v[0], {
      args: ["vm", "planck.img", "4194304", "0", String(HEAP_BYTES)],
      files: files,
      stdin: ev.data.mode + "\n" + ev.data.text
    });
    postMessage({ status: r.status, stdout: r.stdout, stderr: r.stderr });
  }).catch(function (e) {
    postMessage({ status: -1, stdout: "", stderr: String(e) });
  });
};
