(in-package #:simple-vhost-proxy)

(defun starts-with? (string prefix)
  (when (< (length prefix) (length string))
    (string= string prefix :end1 (length prefix))))

(defun maybe-header-value (header-line header)
  (when (starts-with? header-line header)
    (subseq header-line (length header))))

(defun crlf (out)
  (write-char #\Return out) (write-char #\Newline out))

(defun process-headers (header-stream client-ip proxy-ip)
  (let (host content-length forwarded-for connection)
    (values
     (with-output-to-string (out)
       (flet ((out (&rest stuff)
                (dolist (x stuff) (princ x out)) (crlf out)))
         (loop with h while (setf h (read-line header-stream)) do
              (progn
                (acond ((string= h "")
                        (loop-finish))
                       ((maybe-header-value h "Host: ")
                        (setf host (subseq it 0 (or (position #\: it)
                                                    (length it))))
                        (out h))
                       ((maybe-header-value h "Content-Length: ")
                        (setf content-length it) (out h))
                       ((maybe-header-value h "Connection: ")
                        (setf connection t) (out "Connection: close"))
                       ((maybe-header-value h "X-Forwarded-For: ")
                        (setf forwarded-for t)
                        (out h ", " proxy-ip))
                       (t (out h))))
            finally (progn (unless forwarded-for
                             (out "X-Forwarded-For: " client-ip ", " proxy-ip))
                           (unless connection
                             (out "Connection: close"))))))
     host
     (when content-length (parse-integer content-length)))))

(defun forward-response (hosts-table browser-stream forward-headers host
                         content-length)
  (declare (ignore content-length)) ;; for now
  (let ((server (cdr (or (assoc host hosts-table :test #'string=)
                         (car hosts-table)))))
    (with-client-socket (server-socket server-stream (car server) (cadr server)
                                       :element-type '(unsigned-byte 8))
      (set-timeout server-socket 2)
      (let ((s (make-flexi-stream server-stream :external-format :latin1)))
        (princ forward-headers s)
        (crlf s)
        (finish-output s))
      (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
        (loop for n = (read-sequence buffer server-stream) while (< 0 n)
           do (write-sequence buffer browser-stream :end n)
           finally (force-output browser-stream))))))

(defun proxy-connection (browser-socket hosts-table)
  (set-timeout browser-socket 2)
  (multiple-value-call #'forward-response
    hosts-table
    (socket-stream browser-socket)
    (process-headers
     (make-flexi-stream (socket-stream browser-socket)
                        :external-format (make-external-format :latin1 :eol-style :crlf))
     (usocket::host-to-hostname (get-peer-address browser-socket))
     (usocket::host-to-hostname (get-local-address browser-socket)))))

(defun start-proxy (port hosts-table)
  (with-server-socket (listener (socket-listen *wildcard-host* port
                                               :reuseaddress t
                                               :backlog 20
                                               :element-type '(unsigned-byte 8)))
    (loop
       (handler-case
           (with-connected-socket (socket (socket-accept listener))
             (proxy-connection socket hosts-table))
         (connection-aborted-error ())))))