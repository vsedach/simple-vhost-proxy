(in-package #:toot-vhost-proxy)

(defvar default-vhost '("103.246.248.162" 80))
(defvar *vhosts* (make-hash-table :test 'equal))

(defmacro vhost (domain)
  `(gethash ,domain *vhosts* default-vhost))

;; for timeouts:
;; (get-internal-real-time)
;; (internal-time-units-per-second

(defvar max-connections 50)
(defvar connection-timeout-ms 2000)
(defvar in-port 8080)

(defvar +latin-1+
  (make-external-format :latin1 :eol-style :lf)
  "A FLEXI-STREAMS external format used for `faithful' input and
output of binary data.")

(defun proxy-test ()
  (with-server-socket (listener (socket-listen *wildcard-host* in-port
                                               :reuseaddress t
                                               :backlog 20
                                               :element-type '(unsigned-byte 8)))
    (loop
       (handler-case
           (with-connected-socket (socket (socket-accept listener))
             (proxy-connection socket))
         (connection-aborted-error ())))))

(defun read-headers (in)
  (with-output-to-string (headers)
    (let ((newline-state nil))
      (loop for c = (write-char (read-char in) headers) do
           (case c
             ((#\Return #\Newline) (if newline-state
                                       (loop-finish)
                                       (setf newline-state t)))
             (t (setf newline-state nil)))))))

(defun starts-with? (string prefix)
  (when (< (length prefix) (length string))
    (string= string prefix :end1 (length prefix))))

(defun maybe-header-value (header-line header)
  (when (starts-with? header-line header)
    (subseq header-line (length header))))

(defun crlf (out)
  (write-char #\Return out) (write-char #\Newline out))

(defun process-headers (headers client-ip proxy-ip)
  (let (host content-length forwarded-for)
    (values
     (with-output-to-string (out)
       (flet ((out (&rest stuff)
                (dolist (x stuff) (princ x out)) (crlf out)))
         (with-input-from-string (in (string-right-trim '(#\Newline)
                                      (remove #\Return headers)))
           (let ((1st-line (read-line in)))
             (write-sequence 1st-line out
                             :start 0 :end (position #\Space 1st-line :from-end t))
             (out " HTTP/1.0"))
           (loop with h while (setf h (read-line in nil)) do
                (progn
                  (acond ((maybe-header-value h "Host: ")
                          (setf host it) (out h))
                         ((maybe-header-value h "Content-Length: ")
                          (setf content-length it) (out h))
                         ((maybe-header-value h "X-Forwarded-For: ")
                          (setf forwarded-for t)
                          (out h ", " proxy-ip))
                         (t (out h))))
                finally (unless forwarded-for
                          (out "X-Forwarded-For: " client-ip ", " proxy-ip))))))
     host
     (when content-length (parse-integer content-length)))))

;;; timeout on the sockets
;;; binary buffers

(defun forward-response (browser-stream forward-headers host content-length)
  (let ((server (vhost host)))
    (with-client-socket (out-socket s1 (car server) (cadr server)
                                    :element-type '(unsigned-byte 8))
      (let ((s (make-flexi-stream s1 :external-format +latin-1+)))
        (princ forward-headers s) (crlf s)
        (finish-output s)
        (loop with l while (setf l (read-line s nil))
             do (princ l browser-stream) (write-char #\Newline browser-stream) finally (force-output browser-stream))))))

(defun proxy-connection (socket)
  (let ((in (make-flexi-stream (socket-stream socket)
                               :external-format +latin-1+)))
    (multiple-value-call #'forward-response
      in
      (process-headers (read-headers in)
                       (usocket::host-to-hostname (get-peer-address socket))
                       (usocket::host-to-hostname (get-local-address socket))))))
