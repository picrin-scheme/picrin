(import (scheme base)
        (srfi 106)
        (picrin test))

(test-begin)

; The number 9600 has no meaning. I just borrowed from Rust.
(define *test-port* 9600)
(define (next-test-port)
  (set! *test-port* (+ *test-port* 1))
  (number->string *test-port*))

(test #f (socket? '()))
(let* ((port (next-test-port))
       (server (make-server-socket port))
       (client (make-client-socket "127.0.0.1" port)))
  (test #t (socket? server))
  (test #t (socket? client)))

(let* ((port (next-test-port))
       (server (make-server-socket port))
       (client (make-client-socket "127.0.0.1" port)))
  (test #t (socket? (socket-accept server))))

(let* ((port (next-test-port))
       (server (make-server-socket port))
       (client (make-client-socket "127.0.0.1" port))
       (conn (socket-accept server)))
  (test 5 (socket-send conn (string->utf8 "hello")))
  (test "hello" (utf8->string (socket-recv client 5))))

(let* ((port (next-test-port))
       (sock (make-server-socket port)))
  (test #t (port? (socket-input-port sock)))
  (test #t (port? (socket-output-port sock))))

(test *ai-canonname* (socket-merge-flags *ai-canonname*))
(test *ai-canonname* (socket-merge-flags *ai-canonname* *ai-canonname*))
(test *ai-canonname* (socket-purge-flags *ai-canonname*))
(test *ai-canonname* (socket-purge-flags (socket-merge-flags *ai-canonname* *ai-all*)
                                         *ai-all*))
(test *ai-canonname* (socket-purge-flags (socket-merge-flags *ai-all* *ai-canonname*)
                                         *ai-all*))

(test *af-inet* (address-family inet))
(test *af-inet6* (address-family inet6))
(test *af-unspec* (address-family unspec))

(test *sock-stream* (socket-domain stream))
(test *sock-dgram* (socket-domain datagram))

(test *ai-canonname* (address-info canoname))
(test *ai-numerichost* (address-info numerichost))
(test *ai-v4mapped* (address-info v4mapped))
(test *ai-all* (address-info all))
(test *ai-addrconfig* (address-info addrconfig))
(test (socket-merge-flags *ai-v4mapped* *ai-addrconfig*)
      (address-info v4mapped addrconfig))

(test *ipproto-ip* (ip-protocol ip))
(test *ipproto-tcp* (ip-protocol tcp))
(test *ipproto-udp* (ip-protocol udp))

(test 0 (message-type none))
(test *msg-peek* (message-type peek))
(test *msg-oob* (message-type oob))
(test *msg-waitall* (message-type wait-all))
(test (socket-merge-flags *msg-oob* *msg-waitall*)
      (message-type oob wait-all))

(test *shut-rd* (shutdown-method read))
(test *shut-wr* (shutdown-method write))
(test *shut-rdwr* (shutdown-method read write))
(test *shut-rdwr* (shutdown-method write read))

(test-end)
