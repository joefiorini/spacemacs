(defun flow-type/call-process-on-buffer-to-string (command)
  (with-output-to-string
    (call-process-region (point-min) (point-max) shell-file-name nil standard-output nil shell-command-switch command)))

(defun flow-type/get-info-from-def (info)
  (let ((type (alist-get 'type info)))
    (if (not (string-equal type "(unknown)"))
        type)))

(defun flow-type/get-reasons-from-def (info)
  (let ((reasons (alist-get 'reasons info)))
    (if (> (length reasons) 0)
        (alist-get 'desc (aref reasons 0)))))


(defun flow-type/colorize-type (text)
  ;; if web-mode is installed, use that to syntax highlight the type information
  (if (configuration-layer/package-usedp 'web-mode)
      (with-temp-buffer
        (web-mode)
        (insert text)
        (web-mode-set-content-type "jsx")
        (buffer-string))
    ;; otherwise, just return the plain text
    text))

(defun flow-type/describe-info-object (obj)
  (let ((info (flow-type/get-info-from-def obj)))
    (if info
        (flow-type/colorize-type info)
      ;; even if the 'info' field is unknown, there's sometimes useful information in the 'reasons' structure.
      (flow-type/get-reasons-from-def obj))))


(defun flow-type/type-at-cursor ()
  (let ((output (flow-type/call-process-on-buffer-to-string
                 (format "%s type-at-pos --retry-if-init=false --json %d %d"
                         (executable-find "flow")
                         (line-number-at-pos) (+ (current-column) 1)))))
    (unless (string-match "\w*flow is still initializing" output)
      (flow-type/describe-info-object (json-read-from-string output)))))

(defun flow-type/enable-eldoc ()
  (if (and buffer-file-name (locate-dominating-file buffer-file-name ".flowconfig"))
      (set (make-local-variable 'eldoc-documentation-function) 'flow-type/type-at-cursor)))

(defun flow-type/jump-to-definition ()
  (interactive)
  (let ((output (flow-type/call-process-on-buffer-to-string
                 (format "%s get-def --json --path %s %d %d"
                         (executable-find "flow")
                         (buffer-file-name)
                         (line-number-at-pos) (+ (current-column) 1)))))
    (let* ((result (json-read-from-string output))
           (path (alist-get 'path result)))
      (if (> (length path) 0)
          (progn
            (find-file path)
            (goto-char (point-min))
            (forward-line (1- (alist-get 'line result)))
            (forward-char (1- (alist-get 'start result))))))))
