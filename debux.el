(defun debux-match-delete-word? (word)
  (let ((match-word))
    (dolist (w '("(dbg " "(dbgn " "(clog " "(clogn ") match-word)
      (if (string= w word)
        (setq match-word w) ))))

(defun debux-toggle-dbg (&optional post-char)
  "Toggle dbg form in Clojure"
  (interactive)

  (let* ((char-begin-pos (point))
         (char-after-point (string (char-after)))
         (prefix (if (string-match "\\.cljs$" (buffer-name))
                   (concat "(clog" post-char " ")
                   (concat "(dbg" post-char " ") )))

    ;; When double-click the left mouse button
    (cond
      ;; If the char-after-point is blank
      ;;   <ex> ' ' --> (dbg )
      ((string-match "[ \n]" char-after-point)
       (insert (concat prefix ")"))
       (backward-char))

      ;; If the char-after-point is not one of the parentheses
      ;;   <ex> abc  --> (dbg abc)
      ((string-match "[^[{(]" char-after-point)
       (insert prefix)
       (nonincremental-re-search-forward "[ \n]")
       (backward-char)
       (insert ")")
       (backward-char))

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
            (backward-char 1)
            (backward-list)
            (indent-sexp)
            (forward-list) )))))))

(defun debux-toggle-dbgn ()
  "Toggle dbgn form in Clojure"
  (interactive)
  (debux-toggle-dbg "n"))

;; mouse-1: the left mouse button
(defun my-clojure-mode-init ()
  "Initializes clojure mode."
  (interactive)

  ;; Disalbes the default global <down-mouse-1> and <C-down-mouse-1> key
  (global-unset-key (kbd "<down-mouse-1>"))
  (global-unset-key (kbd "<C-down-mouse-1>"))

  (local-set-key (kbd "<double-mouse-1>") 'debux-toggle-dbg)
  (local-set-key (kbd "C-<double-mouse-1>") 'debux-toggle-dbgn))

(add-hook 'clojure-mode-hook 'my-clojure-mode-init)
