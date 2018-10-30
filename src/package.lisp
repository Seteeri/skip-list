;;;; package.lisp

(declaim (optimize (safety 0) (debug 0) (speed 3)))

(defpackage :skip-list
  (:use #:cl)
  (:shadow #:nth
	   #:delete
	   #:length)
  (:export #:skip-list
           #:node
           #:init-skip-list
	   #:find
           #:get-nth-data
           #:set-nth-data
           #:insert
           #:delete
	   #:nth
	   #:doskiplist
	   
	   #:sl-length
	   #:sl-node-head
	   #:ne-forwards
	   #:ne-data

	   #:test))
