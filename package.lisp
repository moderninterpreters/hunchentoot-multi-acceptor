;;;; package.lisp

(defpackage #:hunchentoot-multi-acceptor
  (:use #:cl
        #:hunchentoot)
  (:export #:multi-acceptor
           #:*default-acceptor*
           :listen-fd
           #:add-acceptor))
