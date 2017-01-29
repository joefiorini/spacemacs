(defun flow-type/call-process-on-buffer-to-string (command)
  (with-output-to-string
    (call-process-region (point-min) (point-max) shell-file-name nil standard-output nil shell-command-switch command)))

(defun flow-type/type-description (info)
  (let ((type (alist-get 'type info)))
    (if (string-equal type "(unknown)")
        (let ((reasons (alist-get 'reasons info)))
          (if (> (length reasons) 0) (alist-get 'desc (aref reasons 0))))
      type)))

(defun flow-type/type-at-cursor ()
  (let ((output (flow-type/call-process-on-buffer-to-string
                 (format "%s type-at-pos --retry-if-init=false --json %d %d"
                         (executable-find "flow")
                         (line-number-at-pos) (+ (current-column) 1)))))
    (unless (string-match "\w*flow is still initializing" output)
      (flow-type/type-description (json-read-from-string output)))))

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
