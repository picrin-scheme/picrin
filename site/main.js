var term;

var worker = new Worker('picrin.js');

worker.addEventListener('message', function(e) {
  var ch = String.fromCharCode(e.data.ch);
  if (ch == "\n") {
    ch = "\r\n";
  }
  term.write(ch);
}, false);

window.addEventListener('load', function() {

  term = new Terminal({cols: 80, rows: 24, useStyle: true, cusorBlink: true});

  term.on('data', function(str) {
    worker.postMessage(str);
  });

  term.on('title', function(title) {
    document.title = title;
  });

  term.open(document.querySelector("#term"));

}, false);


