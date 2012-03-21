(in-package #:simple-vhost-proxy)

;;; borrowed from hunchentoot, but this should really go into usocket!!!

(defun set-timeout (socket timeout) ;; in seconds
  #+:clisp
  (socket:socket-options (usocket:socket socket)
                         :SO-RCVTIMEO timeout :SO-SNDTIMEO timeout)
  #+:openmcl
  (progn
    (setf (ccl:stream-input-timeout (usocket:socket socket)) timeout
          (ccl:stream-output-timeout (usocket:socket socket)) timeout))
  #+:sbcl
  (setf (sb-impl::fd-stream-timeout (usocket:socket-stream socket))
        (coerce timeout 'single-float))
  #+:cmu
  (setf (lisp::fd-stream-timeout (usocket:socket-stream socket))
        (coerce timeout 'integer))
  #-(or :clisp :openmcl :sbcl :cmu)
  (warn "Socket timeouts not implemented!"))