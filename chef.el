(require 'dash)
(require 's)
(require 'rsa (concat (file-name-directory (buffer-file-name)) "rsa.el"))

(defun hash-it (s)
  (base64-encode-string (secure-hash 'sha1 s)))

(defun can-it (sep1 sep2 lst)
  "Joins every two elements in list with SEP1, and joins the results with SEP2."
  (s-join sep2
          (mapcar (lambda (tuple) (s-join sep1 tuple))
                  (-partition 2 lst))))

(let* ((chef-server-url "http://10.98.68.204:4000")
       (endpoint "/clients")
       (path (concat chef-server-url endpoint))
       (user-name "admin")
       (method "GET")
       (body "")
       (hashed-path (hash-it endpoint))
       (hashed-body (hash-it body))
       (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
       (canonical_request (can-it ":" "\n"
                                  (list "Method"  method
                                        "Hashed Path"  hashed-path
                                        "X-Ops-Content-Hash"  hashed-body
                                        "X-Ops-Timestamp"  timestamp
                                        "X-Ops-UserId"  user-name))))
  canonical_request)
