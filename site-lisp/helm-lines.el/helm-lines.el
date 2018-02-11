;;; helm-lines.el -*- lexical-binding: t; -*-

(defun helm-lines-action (line)
  "Helm action to insert the selected line at the beginning
   of the current line. Intents the line after inserting it."
  (move-beginning-of-line 1)
  (when (not (looking-at-p "$"))
    (kill-line))
  (insert line)
  (indent-for-tab-command))

(defvar helm-lines-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'helm-next-line)
    (define-key map (kbd "C-p") 'helm-previous-line)
    map)
  "Keymap used in helm project lines.")

(defun helm-lines-async-shell-command (cmd)
  "Execute `NAME'd shell `CMD' async"
  (let ((name "helm-lines" ))
    (start-process-shell-command name (format "*%s*" name) cmd)))

(defun helm-lines-candidates ()
  "Helm candidates by listing all lines under the current git root."
  (let* ((rev-str (shell-command-to-string "git rev-parse --show-toplevel"))
         (git-root (replace-regexp-in-string "\r?\n" "" rev-str))
         (query (if (string-empty-p helm-pattern) "^.*$" helm-pattern)))
    (helm-lines-async-shell-command (format (concat "ag"
                                                    " --nocolor"
                                                    " --nonumbers"
                                                    " --nofilename "
                                                    " --ignore .git"
                                                    " --ignore target"
                                                    " --ignore node_modules"
                                                    " -i \"%s\""                ;; the pattern
                                                    " %s"                       ;; the folder
                                                    " | grep -Ev \"^$\""        ;; remove empty lines
                                                    " | sed -E \"s/^[ \t]*//\"" ;; remove leading ws
                                                    " | sort -u"                ;; unique
                                                    )
                                            (shell-quote-argument query)
                                            (shell-quote-argument git-root)))))


(with-eval-after-load 'helm
  (defvar helm-lines-source
    (helm-build-async-source "Complete line in project"
      :candidates-process 'helm-lines-candidates
      :action 'helm-lines-action))

  (defun helm-lines ()
    (interactive)
    (let ((current-line (replace-regexp-in-string "\r?\n" "" (thing-at-point 'line t))))
      (helm :sources '(helm-lines-source)
            :input current-line
            :keymap helm-lines-map))))

(provide 'helm-lines)
