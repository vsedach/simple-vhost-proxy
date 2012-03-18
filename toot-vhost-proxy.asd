(asdf:defsystem :toot-vhost-proxy
    :name "toot-vhost-proxy"
    :description "HTTP reverse proxy for virtual host web servers in separate processes."
    :author "Vladimir Sedach <vsedach@gmail.com>"
    :license "AGPLv3"
    :serial t
    :components ((:file "package")
                 (:file "proxy"))
    :depends-on (#:usocket #:flexi-streams #:anaphora))