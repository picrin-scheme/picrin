; A R7RS port of "simple echo server" example in SRFI 106
;
; Copyright (C) Takashi Kato (2012). All Rights Reserved.
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(import (scheme base)
        (srfi 106))

(define echo-server-socket (make-server-socket "5000"))

(define (server-run)
  (define (get-line-from-binary-port bin)
    (utf8->string
      (call-with-port (open-output-bytevector)
        (lambda (out)
          (let loop ((b (read-u8 bin)))
            (case b
              ((10) (get-output-bytevector out))
              ((13) (loop (read-u8 bin)))
              (else (write-u8 b out) (loop (read-u8 bin)))))))))

  (call-with-socket (socket-accept echo-server-socket)
    (lambda (sock)
      (let ((in (socket-input-port sock))
            (out (socket-output-port sock)))
        (let loop ((r (get-line-from-binary-port in)))
          (write-bytevector (string->utf8 (string-append r "\r\n")) out)
          (loop (get-line-from-binary-port in)))))))

(server-run)
