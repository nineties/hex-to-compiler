// planck-vm.js: runs a module of the wasm32 target of planck (vm.wasm, the planckvm) in a browser, in a worker or in node.
// The module imports the services of the system from "os" (planck/lib/backend/wasm.pk); this file gives them a memory of files and the standard streams.
//   PlanckVM.run(module, {args, files, stdin}) runs _start of the compiled module (a WebAssembly.Module) and gives {status, stdout, stderr}.
//   args are the arguments of the program (args[0] is its name), files is an object from the paths to the bytes (Uint8Array) of the files that
//   the program may open, and stdin is the text of the standard input. The files that the program creates are added to files.
(function (root) {
  "use strict";

  function Exit(status) {
    this.status = status;
  }

  var encoder = new TextEncoder();
  var decoder = new TextDecoder();

  function bytesOf(x) {
    if (x === undefined || x === null) return new Uint8Array(0);
    return typeof x === "string" ? encoder.encode(x) : x;
  }

  function join(chunks) {
    var n = 0, i, k = 0;
    for (i = 0; i < chunks.length; i++) n += chunks[i].length;
    var out = new Uint8Array(n);
    for (i = 0; i < chunks.length; i++) {
      out.set(chunks[i], k);
      k += chunks[i].length;
    }
    return out;
  }

  function run(module, opts) {
    opts = opts || {};
    var args = opts.args || ["vm"];
    var files = opts.files || {};
    var input = bytesOf(opts.stdin);
    var inputAt = 0;
    var out = [], err = [];
    var fds = {};
    var nextFd = 3;
    var instance = null;

    function u8() {
      return new Uint8Array(instance.exports.memory.buffer);
    }
    function cstring(p) {
      var m = u8(), e = p;
      while (m[e] !== 0) e++;
      return decoder.decode(m.subarray(p, e));
    }

    var os = {
      write: function (fd, buf, n) {
        var bytes = u8().slice(buf, buf + n);
        if (fd === 1) out.push(bytes);
        else if (fd === 2) err.push(bytes);
        else if (fds[fd] && fds[fd].chunks) fds[fd].chunks.push(bytes);
        else return -9;
        return n;
      },
      read: function (fd, buf, n) {
        var src, at;
        if (fd === 0) {
          src = input;
          at = inputAt;
        } else if (fds[fd] && fds[fd].data) {
          src = fds[fd].data;
          at = fds[fd].at;
        } else return -9;
        var k = Math.min(n, src.length - at);
        u8().set(src.subarray(at, at + k), buf);
        if (fd === 0) inputAt += k;
        else fds[fd].at += k;
        return k;
      },
      open: function (path) {
        var name = cstring(path);
        if (!Object.prototype.hasOwnProperty.call(files, name)) return -2;
        var fd = nextFd++;
        fds[fd] = { data: bytesOf(files[name]), at: 0 };
        return fd;
      },
      create: function (path) {
        var fd = nextFd++;
        fds[fd] = { name: cstring(path), chunks: [] };
        return fd;
      },
      close: function (fd) {
        var f = fds[fd];
        if (!f) return -9;
        if (f.chunks) files[f.name] = join(f.chunks);
        delete fds[fd];
        return 0;
      },
      exit: function (status) {
        throw new Exit(status);
      },
      args: function () {
        var strs = args.map(function (a) { return encoder.encode(a); });
        var size = 4 * (strs.length + 1);
        strs.forEach(function (s) { size += s.length + 1; });
        var p = instance.exports.sbrk(size);
        var view = new DataView(instance.exports.memory.buffer);
        var m = u8();
        var at = p + 4 * (strs.length + 1);
        view.setUint32(p, strs.length, true);
        strs.forEach(function (s, i) {
          view.setUint32(p + 4 * (i + 1), at, true);
          m.set(s, at);
          m[at + s.length] = 0;
          at += s.length + 1;
        });
        return p;
      },
      abort: function () {
        throw new Exit(134);
      }
    };

    var status;
    instance = new WebAssembly.Instance(module, { os: os });
    try {
      instance.exports._start();
      status = 0;
    } catch (e) {
      if (e instanceof Exit) status = e.status;
      else {
        err.push(encoder.encode(String(e) + "\n"));
        status = 134;
      }
    }
    return { status: status, stdout: decoder.decode(join(out)), stderr: decoder.decode(join(err)), files: files };
  }

  root.PlanckVM = { run: run };
})(typeof globalThis !== "undefined" ? globalThis : this);
