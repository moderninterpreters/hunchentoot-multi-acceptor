;;;; hunchentoot-multi-acceptor.lisp

(in-package #:hunchentoot-multi-acceptor)

(defclass multi-acceptor (acceptor)
  ((sub-acceptors :initform nil :accessor sub-acceptors)
   (listen-fd :initarg :listen-fd
              :initform nil
              :accessor listen-fd)))

(defparameter *default-acceptor*
  (make-instance 'hunchentoot:easy-acceptor
                 :port 1
                 :name 'default-acceptor))

;; (define-easy-handler (s-default-message :uri "/foo" :acceptor-names '(default-acceptor)) ()
;;   (format nil "error, hostname not set up for: ~a" (host *request*)))


(defmethod acceptor-dispatch-request ((acceptor multi-acceptor) request)
  (let ((host (car (str:split ":" (host request)))))
    (loop for sub in (sub-acceptors acceptor)
       if (equal (car sub) host)
       do (return-from acceptor-dispatch-request
            (let ((*acceptor* (cdr sub)))
              (acceptor-dispatch-request (cdr sub) request))))

    (format *error-output* "oops, nothing available for ~a~%" host)
    ;; for whatever reason this doesn't work at this point. :(
    (acceptor-dispatch-request *default-acceptor* request)))

(defun listen-on-fd (fd &key element-type)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
                             :type :stream
                             :protocol :tcp
                             :descriptor fd)))
    (format t "what the ~A~%" (sb-bsd-sockets::socket-file-descriptor sock))
    (usocket::make-stream-server-socket sock :element-type element-type)))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* ((local (when host
                  (car (usocket:get-hosts-by-name (usocket:host-to-hostname host)))))
         (ipv6 (and local (= 16 (length local))))
         (reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (ip (if (and local (not (eq host usocket:*wildcard-host*)))
                 local
                 (usocket:hbo-to-vector-quad sb-bsd-sockets-internal::inaddr-any)))
         (sock (make-instance (if ipv6
                                         'sb-bsd-sockets::inet6-socket
                                         'sb-bsd-sockets:inet-socket)
                              :type :stream
                              :protocol :tcp)))
    (handler-case
        (usocket:with-mapped-conditions ()
          (setf (sb-bsd-sockets:sockopt-reuse-address sock) reuseaddress)
          (sb-bsd-sockets:socket-bind sock ip port)
          (format t "before the ~A~%" (sb-bsd-sockets::socket-file-descriptor sock))
          (sb-bsd-sockets:socket-listen sock backlog)
          (let ((fd (sb-bsd-sockets::socket-file-descriptor sock)))
            (listen-on-fd fd :element-type element-type)))

      (t (c)
        ;; Make sure we don't leak filedescriptors
        (sb-bsd-sockets:socket-close sock)
        (error c)))))

(defmethod start-listening ((acceptor multi-acceptor))
  (when (hunchentoot::acceptor-listen-socket acceptor)
    (hunchentoot-error "acceptor ~A is already listening" acceptor))
  (setf (hunchentoot::acceptor-listen-socket acceptor)
        (cond
          ((listen-fd acceptor)
           (listen-on-fd (listen-fd acceptor) :element-type '(unsigned-byte 8)))
          (t (socket-listen (or (acceptor-address acceptor)
                                 usocket:*wildcard-host*)
                             (acceptor-port acceptor)
                             :reuseaddress t
                             :backlog (acceptor-listen-backlog acceptor)
                             :element-type '(unsigned-byte 8)))))
  (values))

;; (setf *ma* (make-instance 'multi-acceptor :port 5001))
;; (start *ma*)

(defun add-acceptor (multi-acceptor host acceptor)
  (push (cons host acceptor) (sub-acceptors multi-acceptor)))
