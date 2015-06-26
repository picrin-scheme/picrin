var stdin_buf = [];

function write(fd, ch) {
  self.postMessage({fd: fd, ch: ch});
}

function read(ch) {
  if (stdin_buf.length > 0) {
    console.log("reading.. buf is ", stdin_buf);
    return stdin_buf.shift();
  } else {
    throw "EAGAIN";
  }
}

self.addEventListener('message', function(e) {
  console.log("received:" + e.data);
  e.data.split("").forEach(function(ch) {
      stdin_buf.push(ch.charCodeAt(0));
  });
  console.log("buf is:");
  console.log(stdin_buf);
}, false);

var Module = {
  preRun: [function() {
    FS.init(read, function(ch) {write("stdout", ch);}, function(ch) {write("stderr", ch)});
  }],
  arguments: [],
};

importScripts('bin/picrin.js');
