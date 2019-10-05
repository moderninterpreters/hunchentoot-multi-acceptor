;;;; package.lisp

(defpackage #:hunchentoot-multi-acceptor
  (:use #:cl)
  (:export #:multi-acceptor
           #:*default-acceptor*
           :listen-fd
           #:add-sub-acceptor))
