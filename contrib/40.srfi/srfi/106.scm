(define-library (srfi 106)
  (import (scheme base)
          (srfi 60)
          (picrin optional))

  ; TODO: Define assq-ref anywhere else.
  (define (assq-ref alist key . opt)
    (cond
      ((assq key alist) => cdr)
      (else (if (null? opt) #f (car opt)))))

  (define (socket-merge-flags flag . flags)
    (if (null? flags)
      flag
      (apply socket-merge-flags (logior (or flag 0) (or (car flags) 0))
             (cdr flags))))

  (define (socket-purge-flags base-flag . flags)
    (if (null? flags)
      base-flag
      (apply socket-purge-flags (logxor (or base-flag 0) (or (car flags) 0))
             (cdr flags))))

  (define (make-client-socket node service . args)
    (let-optionals* args ((family *af-inet*)
                          (type *sock-stream*)
                          (flags (socket-merge-flags *ai-v4mapped*
                                                     *ai-addrconfig*))
                          (protocol *ipproto-ip*))
      (make-socket node service family type flags protocol)))

  (define (make-server-socket service . args)
    (let-optionals* args ((family *af-inet*)
                          (type *sock-stream*)
                          (flags *ai-passive*)
                          (protocol *ipproto-ip*))
      (make-socket "" service family type flags protocol)))

  (define %address-family `((inet .   ,*af-inet*)
                            (inet6 .  ,*af-inet6*)
                            (unspec . ,*af-unspec*)))

  (define %socket-domain `((stream .   ,*sock-stream*)
                           (datagram . ,*sock-dgram*)))

  (define %address-info `((canoname .    ,*ai-canonname*)
                          (numerichost . ,*ai-numerichost*)
                          (v4mapped .    ,*ai-v4mapped*)
                          (all .         ,*ai-all*)
                          (addrconfig .  ,*ai-addrconfig*)))

  (define %ip-protocol `((ip .  ,*ipproto-ip*)
                         (tcp . ,*ipproto-tcp*)
                         (udp . ,*ipproto-udp*)))

  (define %message-types `((none .      0)
                           (peek .     ,*msg-peek*)
                           (oob .      ,*msg-oob*)
                           (wait-all . ,*msg-waitall*)))

  (define-syntax address-family
    (syntax-rules ()
      ((_ name)
       (assq-ref %address-family 'name))))

  (define-syntax socket-domain
    (syntax-rules ()
      ((_ name)
       (assq-ref %socket-domain 'name))))

  (define-syntax address-info
    (syntax-rules ()
      ((_ names ...)
       (apply socket-merge-flags
              (map (lambda (name) (assq-ref %address-info name))
                   '(names ...))))))

  (define-syntax ip-protocol
    (syntax-rules ()
      ((_ name)
       (assq-ref %ip-protocol 'name))))

  (define-syntax message-type
    (syntax-rules ()
      ((_ names ...)
       (apply socket-merge-flags
              (map (lambda (name) (assq-ref %message-types name))
                   '(names ...))))))

  (define (%shutdown-method names)
    (define (state->method state)
      (case state
        ((read) *shut-rd*)
        ((write) *shut-wr*)
        ((read-write) *shut-rdwr*)
        (else #f)))
    (let loop ((names names)
               (state 'none))
      (cond
        ((null? names) (state->method state))
        ((eq? (car names) 'read)
         (loop (cdr names)
               (cond
                 ((eq? state 'none) 'read)
                 ((eq? state 'write) 'read-write)
                 (else state))))
        ((eq? (car names) 'write)
         (loop (cdr names)
               (cond
                 ((eq? state 'none) 'write)
                 ((eq? state 'read) 'read-write)
                 (else state))))
        (else (loop (cdr names) 'other)))))

  (define-syntax shutdown-method
    (syntax-rules ()
      ((_ names ...)
       (%shutdown-method '(names ...)))))

  ;; Constructors and predicate
  (export make-client-socket
          make-server-socket
          socket?)

  ;; Socket operations
  (export socket-accept
          socket-send
          socket-recv
          socket-shutdown
          socket-close)

  ;; Port conversion
  (export socket-input-port
          socket-output-port)

  ;; Control feature
  (export call-with-socket)

  ;; Flag operations
  (export address-family
          socket-domain
          address-info
          ip-protocol
          message-type
          shutdown-method
          socket-merge-flags
          socket-purge-flags)

  ;; Constant values
  (export *af-inet*
          *af-inet6*
          *af-unspec*)
  (export *sock-stream*
          *sock-dgram*)
  (export *ai-canonname*
          *ai-numerichost*
          *ai-v4mapped*
          *ai-all*
          *ai-addrconfig*)
  (export *ipproto-ip*
          *ipproto-tcp*
          *ipproto-udp*)
  (export *msg-peek*
          *msg-oob*
          *msg-waitall*)
  (export *shut-rd*
          *shut-wr*
          *shut-rdwr*))
