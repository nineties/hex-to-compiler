// test_playground.js: runs the examples of the tutorial in the PlanckVM of the browser (docs/assets/vm.wasm and planck.img) and compares the
// output with the recorded output of examples/NAME.out.
// usage (in the root of the repository): node docs/tools/test_playground.js [NAME ...]
// The examples are the marks <!--ex NAME--> and <!--repl NAME--> of the pages of docs/tools/src/ja, the ones that get a button to run.
"use strict";
const fs = require("fs");
const path = require("path");

const root = path.join(__dirname, "..", "..");
require(path.join(root, "docs", "assets", "planck-vm.js"));
const mod = new WebAssembly.Module(fs.readFileSync(path.join(root, "docs", "assets", "vm.wasm")));
const image = new Uint8Array(fs.readFileSync(path.join(root, "docs", "assets", "planck.img")));

const src = path.join(root, "docs", "tools", "src", "ja");
const examples = [];
const seen = new Set();
for (const d of fs.readdirSync(src, { withFileTypes: true }).sort((a, b) => (a.name < b.name ? -1 : 1))) {
  if (!d.isDirectory()) continue;
  for (const f of fs.readdirSync(path.join(src, d.name)).sort()) {
    const text = fs.readFileSync(path.join(src, d.name, f), "utf8");
    const re = /<!--(ex|repl) ([A-Za-z0-9_]+)-->/g;
    let m;
    while ((m = re.exec(text)) !== null) {
      if (seen.has(m[1] + m[2])) continue;
      seen.add(m[1] + m[2]);
      examples.push({ kind: m[1], name: m[2] });
    }
  }
}

const only = process.argv.slice(2);
let failed = 0;
let count = 0;
for (const { kind, name } of examples) {
  if (only.length > 0 && !only.includes(name)) continue;
  const base = path.join(root, "docs", "tools", "examples", name);
  const code = fs.readFileSync(base + (kind === "ex" ? ".pk" : ".repl"), "utf8");
  const expected = fs.readFileSync(base + ".out", "utf8");
  const r = PlanckVM.run(mod, {
    args: ["vm", "planck.img", "4194304", "0", "134217728"],
    files: { "planck.img": image },
    stdin: (kind === "ex" ? "run\n" : "repl\n") + code,
  });
  const actual = r.stdout + (r.status !== 0 ? r.stderr : "");
  count++;
  if (actual.trimEnd() === expected.trimEnd()) {
    console.log(`ok ${name}`);
  } else {
    failed++;
    console.log(`NG ${name}`);
    console.log("--- expected\n" + expected + "--- actual\n" + actual);
  }
}
console.log(`${count - failed}/${count} examples have the recorded output`);
process.exit(failed === 0 ? 0 : 1);
