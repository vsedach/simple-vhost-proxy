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

(defun forward-bytes (from to &key how-many)
  (let ((buffer (make-array (min (or how-many 2048) 2048)
                            :element-type '(unsigned-byte 8)))
        (so-far 0))
    (loop for n = (read-sequence buffer from) do
         (when (= 0 n) (loop-finish))
         (write-sequence buffer to :end n)
         (when (and how-many (<= how-many (incf so-far n))) (loop-finish)))
    (force-output to)))

(defun forward-response (hosts-table browser-stream forward-headers host
                         content-length server-timeout)
  (let ((server (cdr (or (assoc host hosts-table :test #'string=)
                         (car hosts-table)))))
    (with-client-socket (server-socket server-stream (car server) (cadr server)
                                       :element-type '(unsigned-byte 8))
      (set-timeout server-socket server-timeout)
      (let ((s (make-flexi-stream server-stream :external-format :latin1)))
        (princ forward-headers s) (crlf s) (finish-output s))
      (when content-length
        (forward-bytes browser-stream server-stream :how-many content-length))
      (forward-bytes server-stream browser-stream))))

(defun proxy-connection (browser-socket hosts-table client-timeout server-timeout)
  (set-timeout browser-socket client-timeout)
  (multiple-value-call #'forward-response
    hosts-table
    (socket-stream browser-socket)
    (process-headers
     (make-flexi-stream
      (socket-stream browser-socket)
      :external-format (make-external-format :latin1 :eol-style :crlf))
     (usocket::host-to-hostname (get-peer-address browser-socket))
     (usocket::host-to-hostname (get-local-address browser-socket)))
    server-timeout))

(defun start-proxy (port hosts-table &key
                    (client-timeout 2)
                    (server-timeout 2))
  "Start a reverse HTTP proxy on the specified port. hosts-table
should consist of a list of hosts to be forwarded to particular IPs
and ports, like so: ((\"domain1\" \"127.0.0.1\" 8080) (\"domain2\"
\"127.0.0.1\" 8081)) The first entry in the hosts-table is the default
forwarding address if none of the entries match

Returns the main proxy server thread.

Optional parameters:
client-timeout (read and write) in seconds
server-timeout (read and write) in seconds"
  (bt:make-thread
   (lambda ()
     (with-server-socket (listener (socket-listen
                                    *wildcard-host* port
                                    :reuseaddress t
                                    :backlog 20
                                    :element-type '(unsigned-byte 8)))
       (loop
          (handler-case
              (let ((socket (socket-accept listener)))
                (bt:make-thread
                 (lambda ()
                   (handler-case
                       (with-connected-socket (socket socket)
                         (proxy-connection socket hosts-table
                                           client-timeout server-timeout))
                     (error ())))
                 :name (format nil "simple-vhost-proxy (port ~A) connection thread"
                               port)))
            (error ()))))) ;; probably should do something less dumb
   :name (format nil "simple-vhost-proxy main thread (port ~A)" port)))
