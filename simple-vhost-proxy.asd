(asdf:defsystem :simple-vhost-proxy
  :name "simple-vhost-proxy"
  :description "HTTP reverse proxy for virtual host web servers."
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :license "AGPLv3"
  :serial t
  :components ((:file "package")
               (:file "timeouts")
               (:file "proxy"))
  :depends-on (#:usocket #:flexi-streams #:anaphora #:bordeaux-threads))
