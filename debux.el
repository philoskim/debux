(defun toggle-dbg (&optional post-char)
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
      ;;  <ex> ' ' --> (dbg )
      ((string-match "[ \n]" char-after-point)
       (insert (concat prefix ")"))
       (backward-char))

      ;; If the char-after-point is not parentheses:  <ex> abc  --> (dbg abc)
      ((string-match "[^[{(]" char-after-point)
       (insert prefix)
       (nonincremental-re-search-forward "[ \n]")
       (backward-char)
       (insert ")")
       (backward-char))

      ;; If the char after-point is parentheses
      ((string-match "[[{(]" char-after-point)
       (set-mark (point))
       (forward-word)
       (forward-char)
       (exchange-point-and-mark)
       (cond
         ;; If the following string is '(dbg ', delete it.
         ;; <ex> (dbg ...) --> ...
         ((string= (buffer-substring (point) (mark)) prefix)
          (forward-list)
          (let ((list-begin-pos (mark)))
            (backward-char)
            (delete-char 1)
            (goto-char list-begin-pos)
            (backward-delete-char (length prefix)) ))

         ;; If the following string is not '(dbg ', insert it.
         ;;  <ex> (...) --> (dbgn (...))
         (t
          (insert prefix)
          (forward-list)
          (insert ")")
          (backward-char 1) ))))))

(defun toggle-dbgn ()
  "Toggle dbgn form in Clojure"
  (interactive)
  (toggle-dbg "n"))

;; mouse-1: the left mouse button
(defun my-clojure-mode-init ()
  "Initializes clojure mode."
  (interactive)

  ;; disalbes the default <C-down-mouse-1> key
  (global-unset-key (kbd "<C-down-mouse-1>"))

  (local-set-key (kbd "<double-mouse-1>") 'toggle-dbg)
  (local-set-key (kbd "C-<double-mouse-1>") 'toggle-dbgn))

(add-hook 'clojure-mode-hook 'my-clojure-mode-init)
