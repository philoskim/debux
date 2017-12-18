(defun debux-match-delete-word? (word)
  (let ((match-word))
    (dolist (w '("(dbg " "(dbgn " "(clog " "(clogn ") match-word)
      (when (string= w word)
        (setq match-word w) ))))

(defun debux-toggle-dbg (&optional post-char)
  (interactive)
  (let* ((char-begin-pos (point))
         (char-after-point (string (char-after)))
         (prefix (if (string-match "\\.cljs$" (buffer-name))
                   (concat "(clog" post-char " ")
                   (concat "(dbg" post-char " ") )))

    ;; When the left mouse button double-clicked
    (cond
      ;; If the char-after-point is not one of the parentheses
      ;;   <ex> abc  --> (dbg abc)
      ((string-match "[^[{(]" char-after-point)
       (insert prefix)
       (nonincremental-re-search-forward "[] \n)}]")
       (backward-char)
       (insert ")")
       (backward-list))

      ;; If the char-after-point is one of the parentheses
      ((string-match "[[{(]" char-after-point)
       (set-mark (point))
       (nonincremental-re-search-forward "[ \n]")
       (exchange-point-and-mark)
       (let ((fn-name (buffer-substring (point) (mark))))
         (cond
           ;; If the following string matches a delete-word, delete it.
           ;; <ex> (dbg ...) --> ...
           ((debux-match-delete-word? fn-name)
            (forward-list)
            (let ((list-begin-pos (mark)))
              (backward-char)
              (delete-char 1)
              (goto-char list-begin-pos)
              (backward-delete-char (length fn-name))
              (indent-sexp) ))

           ;; If the following string doesn't match a delete-word, insert the prefix.
           ;;  <ex> (...) --> (dbg (...))
           (t
            (insert prefix)
            (forward-list)
            (insert ")")
            (backward-list)
            (indent-sexp) )))))))

(defun debux-toggle-dbgn ()
  (interactive)
  (debux-toggle-dbg "n"))

(defun debux-down-mouse-1 (orig-fun &rest args)
  (let ((res (apply orig-fun args)))
    (when (eq major-mode 'clojure-mode)
      (set-mark-command (point)))
    res))

(defun my-clojure-mode-init ()
  "Initializes clojure mode."
  (interactive)

  ;; mouse-1: the left mouse button
  ;; Disables the default global <down-mouse-1> and <C-down-mouse-1> key
  (global-unset-key (kbd "<C-down-mouse-1>"))
  (advice-add 'mouse-drag-region :around #'debux-down-mouse-1)

  (local-set-key (kbd "<double-mouse-1>") 'debux-toggle-dbg)
  (local-set-key (kbd "C-<double-mouse-1>") 'debux-toggle-dbgn))

(add-hook 'clojure-mode-hook 'my-clojure-mode-init)
