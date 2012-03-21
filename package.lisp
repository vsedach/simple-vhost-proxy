(in-package #:cl)

(defpackage #:simple-vhost-proxy
  (:use #:cl #:usocket #:flexi-streams #:anaphora)
  (:export #:start-proxy))
