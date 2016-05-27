(require 'dash)
(require 's)
(require 'request)

(defvar chef-config (make-hash-table :test 'equal)
  "A hashtable containing all configuration values this system
  requires. Call `load-chef-config' to use the values in a
  standard Ruby-based Chef configuration file.")

(defun chef--config-url ()
  "Return configured URL to Chef server."
  (gethash "chef_server_url" chef-config "http://localhost:4000"))

(defun chef--config-username ()
  "Return configured `node_name' or `admin' if not configured."
  (gethash "node_name" chef-config "admin"))

(defun chef--config-pemfile ()
  "Return configured PEM file or the PEM file that matches the username."
  (gethash "client_key" chef-config
           (concat (getenv "HOME") "/.chef/" (chef--config-username) ".pem")))

(defun chef--config-load-line (line)
  "Given a LINE that matches a key value expression, add this
value (modified perhaps) to the `chef-config' hashtable. Yeah, a
function that ain't very functional."
  (when (string-match "^\\([A-z_]*\\) +\"\\(.*\\)\"" line)
    (let ((key (match-string 1 line))
          (val (match-string 2 line)))

      ;; If the client key is relative, prepend parent dir
      (if (and (equal key "client_key")
               (not (equal "/" (substring val 0 1))))
          (setq val (directory-file-name config-file)))

      ;; Add any proxy entries to the URL proxy list...
      ;; TODO: Add, not just set ...
      (if (equal key "http_proxy")
          (setq url-proxy-services '(("http" . val))))
      (if (equal key "https_proxy")
          (setq url-proxy-services '(("https" . val))))

      (puthash key val chef-config))))

(defun chef-config-load (config-file)
  "Loads a Chef configuration file, CONFIG-FILE, and populates
the internal hash table, looking for the keys, `chef_server_url',
`node_name' and `client_key'."
  (interactive "fChef configuation file: ")
  (clrhash chef-config)
  (with-temp-buffer
    (insert-file-contents config-file)
    (mapcar 'chef--config-load-line
            (split-string (buffer-string) "\n" t))))



(defun chef--hash-encode (s)
  "Given a string, S, encode it with SHA1 and Base64."
  (base64-encode-string (secure-hash 'sha1 s)))

(defun chef--convert-to-canonical-payload (sep1 sep2 lst)
  "Joins every two elements in list with SEP1, and joins the
results with SEP2."
  (s-join sep2
          (mapcar (lambda (tuple) (s-join sep1 tuple))
                  (-partition 2 lst))))

(defun chef--rsa-sign-string (pemfile contents)
  "Use the PEMFILE to sign the CONTENTS string.

TODO: Need to delete the temporary file when complete."

  (let ((infile "/tmp/chef-signed-contents.txt"))
    (with-temp-file infile
      (insert contents))
    (with-temp-buffer
      (toggle-enable-multibyte-characters)
      (call-process "openssl" infile t nil
                    "rsautl" "-sign" "-inkey" pemfile)
      (buffer-string))))

(defun chef--index-auth-header (index line)
  "With a zero-based INDEX, return an `X-Ops-Authorization' line
with a 1-based value associated with the LINE."
  (let ((header-line (concat "X-Ops-Authorization-"
                             (int-to-string (1+ index)))))
    (cons header-line line)))

(defun chef--index-auth-headers (encrypted-string)
  "Given an ENCRYPTED-STRING, return a list of cons'd header
values that have been broken up and given index authorization
lines, e.g. `X-Ops-Authorization-1'."
  (-map-indexed 'chef--index-auth-header
                (s-split "\n" encrypted-string)))


(defun chef-request (endpoint &optional body)
  (let* ((method "GET")
         (path (concat (chef--config-url) endpoint))
         (user-name (chef--config-username))
         (hashed-path (chef--hash-encode endpoint))
         (hashed-body (chef--hash-encode (or body "")))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ"))
         (keypem (chef--config-pemfile))
         (canonical-request (chef--convert-to-canonical-payload
                             ":" "\n"
                             (list "Method"  method
                                   "Hashed Path"  hashed-path
                                   "X-Ops-Content-Hash"  hashed-body
                                   "X-Ops-Timestamp"  timestamp
                                   "X-Ops-UserId"  user-name)))
         (auth-headers (base64-encode-string
                        (chef--rsa-sign-string keypem canonical-request)))

         (all-headers (append `(("Accept" . "application/json")
                                ("X-Ops-Timestamp" . ,timestamp)
                                ("X-Ops-Userid" . ,user-name)
                                ("X-Chef-Version" . "0.10.4")
                                ("X-Ops-Content-Hash" . ,hashed-body)
                                ("X-Ops-Sign" . "version=1.0"))
                              (chef--index-auth-headers auth-headers))))

    ;; At this point, we have our headers, and are ready to make a
    ;; connection to the Chef server. We have two approaches, using
    ;; the Emacs URL:

    ;; (let ((url-request-method method)
    ;;       (url-request-extra-headers all-headers)
    ;;       (url-proxy-services '(("http" . "localhost:1080")
    ;;                             ("https" . "localhost:1080"))))
    ;;   (url-retrieve path (lambda (status) (switch-to-buffer (current-buffer)))))

    ;; Or, we can use Emacs Request package (which doesn't claim to
    ;; use the http proxy:

    (request path
             :type method
             :sync t
             :headers all-headers
             :parser 'json-read
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (message "Sent: %S" (assoc-default 'json data)))))
    ))

;; Try it out ... first, load the Knife Configuration File:
(chef-config-load (concat (getenv "HOME") "/.chef/knife.rb"))

(chef-request "/nodes")
