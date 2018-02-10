;;; helm-insert-line-from-project.el -*- lexical-binding: t; -*-

(defun t/helm-project-lines-action (line)
  "Helm action to insert the selected line at the beginning
   of the current line. Intents the line after inserting it."
  (move-beginning-of-line 1)
  (when (not (looking-at-p "$"))
    (kill-line))
  (insert line)
  (indent-for-tab-command))

(defvar t/helm-project-lines-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'helm-next-line)
    (define-key map (kbd "C-p") 'helm-previous-line)
    map)
  "Keymap used in helm project lines.")

(defun t/helm-project-lines-candidates ()
  "Helm candidates by listing all lines under the current git root."
  (let* ((git-root (replace-regexp-in-string
                    "\r?\n"
                    ""
                    (shell-command-to-string "git rev-parse --show-toplevel")))
         (query (if (string-empty-p helm-pattern)
                    "^.*$"
                  helm-pattern)))
    (t/async-shell-command "helm-project-lines"
                           (format (concat "ag"
                                           " --nocolor"
                                           " --nonumbers"
                                           " --nofilename "
                                           " --ignore .git"
                                           " --ignore target"
                                           " --ignore node_modules"
                                           " -i \"%s\"" ;; the pattern
                                           " %s" ;; the folder
                                           " | grep -Ev \"^$\"" ;; remove empty lines
                                           " | sed -E \"s/^[ \t]*//\"" ;; remove leading ws
                                           " | sort -u" ;; unique
                                           )
                                   (shell-quote-argument query)
                                   (shell-quote-argument git-root)))))

(defun t/init-helm-source-project-lines ()
  (defvar t/helm-source-project-lines
    (helm-build-async-source "Complete line in project"
      :candidates-process 't/helm-project-lines-candidates
      :action 't/helm-project-lines-action))

  (defun t/helm-find-and-insert-line-from-project ()
    (interactive)
    (let ((current-line-string (replace-regexp-in-string "\r?\n" "" (thing-at-point 'line t))))
      (helm :sources '(t/helm-source-project-lines)
            :input current-line-string
            :keymap t/helm-project-lines-keymap))))

(t/after helm
  (t/init-helm-source-project-lines))

(provide 'helm-insert-line-from-project)
