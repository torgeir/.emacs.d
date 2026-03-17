;;; init.el --- Init -*- lexical-binding: t; -*-
(message "Yo.")

;;; system checks
(defconst is-mac (eq system-type 'darwin))
(defconst is-linux (eq system-type 'gnu/linux))
(defconst is-bsd (memq system-type '(darwin gnu/kfreebsd)))
(defconst is-windows (memq system-type '(windows-nt ms-dos cygwin)))

;;; early init - does this really need its own file?
;; temporarily raise gc limits during startup, restore after.
(defvar t--backup-gc-cons-threshold gc-cons-threshold)
(defvar t--backup-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold t--backup-gc-cons-threshold)
            (setq gc-cons-percentage t--backup-gc-cons-percentage))
          100)

;; reduce file-name handler overhead during startup, restore after.
(defvar t--old-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist t--old-file-name-handler-alist))
          101)

;; improve throughput for subprocess i/o (lsp, ripgrep, git).
(setq read-process-output-max (* 2 1024 1024))

;;; packages-diy
(require 'subr-x)
(require 'cl-lib)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(defvar t-package-queue nil)
(defvar t-package-order nil)
(defvar t-package-status (make-hash-table :test #'equal))
(defvar t-package-meta (make-hash-table :test #'equal))
(defvar t-package-registry nil)
(defvar t-package-status-buffer "*t-packages*")
(defvar t-package-log-buffer "*t-packages-log*")
(defvar t-package-in-progress nil)
(defvar t-package-active (make-hash-table :test #'equal))
(defvar t-package-max-parallel 13)
(defvar t-package-complete-announced nil)
(defvar t-package-install-complete-hook nil)
(defvar t--reload-in-progress nil)
(defvar t-package-pending-activation nil)
(defvar t-spinner-frames '("|" "/" "-" "\\"))
(defvar t-spinner-index 0)
(defvar t-spinner-timer nil)

(defun t--repo-url (host repo)
  (concat (pcase host
            ('gh "https://github.com/")
            ('gl "https://gitlab.com/")
            ('cb "https://codeberg.org/")
            ('sav "https://git.savannah.gnu.org/git/")
            (_ (error (concat "Unknown host, got " host))))
          repo))

(defun t--repo-web-url (host repo)
  (let ((clean-repo (string-remove-suffix ".git" repo)))
    (concat (pcase host
              ('gh "https://github.com/")
              ('gl "https://gitlab.com/")
              ('cb "https://codeberg.org/")
              ('sav "https://git.savannah.gnu.org/git/")
              (_ (error (concat "Unknown host, got " host))))
            clean-repo)))

(defun t--repo-commit-url (host repo rev)
  (let ((clean-repo (string-remove-suffix ".git" repo)))
    (pcase host
      ('gh (format "https://github.com/%s/commit/%s" clean-repo rev))
      ('gl (format "https://gitlab.com/%s/-/commit/%s" clean-repo rev))
      ('cb (format "https://codeberg.org/%s/commit/%s" clean-repo rev))
      ('sav (format "https://git.savannah.gnu.org/git/%s?a=commit;h=%s" clean-repo rev))
      (_ (error (concat "Unknown host, got " host))))))

(defun t--package-dir (name)
  (expand-file-name (symbol-name name) package-user-dir))

(defun t--package-head-file (pkg-dir)
  (expand-file-name ".pin-rev" pkg-dir))

(defun t--package-up-to-date-p (pkg-dir rev)
  (let ((head-file (t--package-head-file pkg-dir)))
    (and (file-directory-p pkg-dir)
         (file-exists-p head-file)
         (string= (string-trim (with-temp-buffer
                                 (insert-file-contents head-file)
                                 (buffer-string)))
                  rev))))

(defun t--package-outdated-p (pkg-dir rev)
  (let ((head-file (t--package-head-file pkg-dir)))
    (and (file-directory-p pkg-dir)
         (file-exists-p head-file)
         (not (t--package-up-to-date-p pkg-dir rev)))))

(defun t--package-load-dir (name subdir)
  (let* ((pkg-dir (t--package-dir name))
         (lisp-dir (expand-file-name "lisp" pkg-dir))
         (name-dir (expand-file-name (symbol-name name) pkg-dir)))
    (cond
     (subdir (expand-file-name subdir pkg-dir))
     ((file-directory-p name-dir) name-dir)
     ((file-directory-p lisp-dir) lisp-dir)
     (t pkg-dir))))

(defun t--package-add-load-path (name subdir)
  (let* ((pkg-dir (t--package-dir name))
         (load-dir (t--package-load-dir name subdir))
         (ext-dirs (delq nil
                         (list
                          (let ((dir (expand-file-name "extensions" pkg-dir)))
                            (when (file-directory-p dir) dir))
                          (let ((dir (expand-file-name "extensions" load-dir)))
                            (when (file-directory-p dir) dir))))))
    (add-to-list 'load-path load-dir)
    (dolist (dir ext-dirs)
      (add-to-list 'load-path dir))))

(defun t--log (fmt &rest args)
  (let ((buf (get-buffer-create t-package-log-buffer)))
    (with-current-buffer buf
      (let ((follow (and (get-buffer-window buf)
                         (= (point) (point-max)))))
        (goto-char (point-max))
        (insert (apply #'format fmt args) "\n")
        (when (and follow (get-buffer-window buf))
          (goto-char (point-max)))))))

(defun t--process-output (name output)
  (let* ((prefix (format "[%s] " name))
         (text (replace-regexp-in-string "\r" "\n" output))
         (lines (split-string text "\n"))
         (buf (get-buffer-create t-package-log-buffer)))
    (with-current-buffer buf
      (let ((follow (and (get-buffer-window buf)
                         (= (point) (point-max)))))
        (goto-char (point-max))
        (dolist (line lines)
          (unless (string-empty-p line)
            (insert prefix line "\n")))
        (when (and follow (get-buffer-window buf))
          (goto-char (point-max)))))))

;; Back-compat for any in-flight processes still using the old filter.
(defun t--process-filter (p output)
  (t--process-output
   (or (and (processp p) (process-get p 't-name)) "git")
   output))

(defun t--status-line (name meta status)
  (let* ((in-progress (member status '("cloning" "checking out")))
         (spin (when in-progress
                 (nth t-spinner-index t-spinner-frames)))
         (label (if spin (format "%s %s" spin status) status))
         (host (or (plist-get meta :host) ""))
         (repo (string-remove-suffix ".git" (or (plist-get meta :repo) "")))
         (rev (or (plist-get meta :rev) "")))
    (format "%-20s %-4s %-28s %-8s %s\n" name host repo rev label)))

(defun t--status-render ()
  (let ((buf (get-buffer-create t-package-status-buffer)))
    (with-current-buffer buf
      (let* ((win (get-buffer-window buf))
             (saved-point (when win (window-point win)))
             (saved-start (when win (window-start win))))
        (erase-buffer)
	      (insert "-- t packages --\n\n")
	      (insert-text-button
	       "install queued"
         'action (lambda (_btn) (t-install-queued-packages))
         'follow-link t
         'help-echo "Install queued packages")
	      (insert " ")
	      (insert-text-button
	       "rescan"
         'action (lambda (_btn) (t-rescan-packages))
         'follow-link t
         'help-echo "Rescan packages")
	      (insert "\n\n")
	      (insert (format "%-20s %-4s %-28s %-8s %s\n"
			                  "name" "prov" "repo" "rev" "status"))
	      (insert (make-string 73 ?-) "\n")
	      (cl-labels
	          ((insert-line
	             (name &optional prefix)
	             (let* ((status (gethash name t-package-status "unknown"))
		                  (meta (gethash name t-package-meta))
		                  (in-progress (member status '("cloning" "checking out")))
		                  (spin (when in-progress
			                        (nth t-spinner-index t-spinner-frames)))
		                  (label (if spin (format "%s %s" spin status) status))
		                  (host (or (plist-get meta :host) ""))
		                  (repo (string-remove-suffix ".git" (or (plist-get meta :repo) "")))
		                  (rev (or (plist-get meta :rev) ""))
		                  (name-text (format "%s%s" (or prefix "") (symbol-name name))))
		             (insert (format "%-20s %-4s %-28s" name-text host repo))
		             (if meta
		                 (let* ((rev-text (format "%-8s" rev))
			                      (url (t--repo-commit-url
				                          (plist-get meta :host)
				                          (plist-get meta :repo)
				                          (plist-get meta :rev))))
		                   (insert
			                  (propertize
			                   (format " %s" rev-text)
			                   'mouse-face 'highlight
			                   'help-echo "Open commit"
			                   'keymap (let ((map (make-sparse-keymap)))
				                           (define-key map [mouse-1]
					                                     (lambda ()
						                                     (interactive)
						                                     (browse-url url)))
				                           map))))
		               (insert (format " %-8s" rev)))
		             (insert (format " %s\n" label)))))
	        (let (top-level)
	          (dolist (name t-package-order)
	            (let* ((meta (gethash name t-package-meta))
		                 (dep-of (plist-get meta :dep-of)))
		            (unless dep-of
		              (push name top-level))))
	          (setq top-level (nreverse top-level))
	          (dolist (name top-level)
	            (insert-line name)
	            (let* ((meta (gethash name t-package-meta))
		                 (deps (plist-get meta :deps)))
		            (dolist (dep deps)
		              (insert-line dep "- "))))))
        (setq buffer-read-only nil)
        (when (and win saved-point saved-start)
          (set-window-point win saved-point)
          (set-window-start win saved-start t))))
    buf))

(defun t--display-status-buffer ()
  (let ((buf (t--status-render)))
    (unless (get-buffer-window buf t)
      (let ((win (display-buffer
                  buf
                  '((display-buffer-reuse-window display-buffer-pop-up-window)
                    (inhibit-same-window . t)))))
        (when (window-live-p win)
          (with-selected-window win
            (goto-char (point-min))
            (deactivate-mark)))))))

(defun t--display-status-and-log-buffers ()
  (let* ((status-buffer (t--status-render))
         (log-buffer (get-buffer-create t-package-log-buffer)))
    (display-buffer
     status-buffer
     '((display-buffer-reuse-window display-buffer-pop-up-window)
       (inhibit-same-window . t)))
    (display-buffer
     log-buffer
     '((display-buffer-reuse-window display-buffer-pop-up-window)
       (inhibit-same-window . t)))))

(defun t--display-status-and-log-buffers-exclusive ()
  (let* ((status-buffer (t--status-render))
         (log-buffer (get-buffer-create t-package-log-buffer)))
    (delete-other-windows)
    (switch-to-buffer status-buffer)
    (goto-char (point-min))
    (let ((log-window (split-window-below)))
      (set-window-buffer log-window log-buffer)
      (set-window-point log-window (point-max))
      (set-window-start log-window (point-max)))))

(defun t--spinner-tick ()
  (setq t-spinner-index
        (mod (1+ t-spinner-index) (length t-spinner-frames)))
  (when t-package-in-progress
    (t--status-render)))

(defun t--spinner-start ()
  (unless t-spinner-timer
    (setq t-spinner-index 0)
    (setq t-spinner-timer
          (run-with-timer 0 0.2 #'t--spinner-tick))))

(defun t--spinner-stop ()
  (when t-spinner-timer
    (cancel-timer t-spinner-timer)
    (setq t-spinner-timer nil)))

(defun t--active-add (name)
  (puthash name t t-package-active))

(defun t--active-remove (name)
  (remhash name t-package-active))

(defun t--active-count ()
  (hash-table-count t-package-active))

(defun t--any-processes-running ()
  (seq-some (lambda (p)
              (let ((name (process-name p)))
                (or (string-prefix-p "t-clone-" name)
                    (string-prefix-p "t-checkout-" name))))
            (process-list)))

(defun t--status-set (name status)
  (puthash name status t-package-status)
  (t--status-render))

(defun t--reset-queued-statuses ()
  (dolist (spec t-package-queue)
    (puthash (plist-get spec :name) "queued" t-package-status)))

(defun t--status-register (name)
  (unless (member name t-package-order)
    (setq t-package-order
          (append t-package-order (list name)))))

(defun t--queue-package (spec)
  (let ((name (plist-get spec :name)))
    (unless (gethash name t-package-active)
      (setq t-package-queue
            (append (seq-remove (lambda (item)
                                  (eq (plist-get item :name) name))
                                t-package-queue)
                    (list spec))))))

(defun t--registry-add (spec)
  (let ((name (plist-get spec :name)))
    (setq t-package-registry
          (cons spec
                (seq-remove (lambda (item)
                              (eq (plist-get item :name) name))
                            t-package-registry)))))

(defun t--registry-find (name)
  (seq-find (lambda (item)
              (eq (plist-get item :name) name))
            t-package-registry))

(defun t--activate-package (spec)
  (let* ((name (plist-get spec :name))
         (deps (plist-get spec :deps)))
    (if (t--deps-installed-p deps)
        (progn
          (t--package-add-load-path name (plist-get spec :subdir))
          (when-let ((form (plist-get spec :use-package-form)))
            (eval (copy-tree form))))
      (setq t-package-pending-activation
            (append
             (seq-remove (lambda (item)
                           (eq (plist-get item :name) name))
                         t-package-pending-activation)
             (list spec)))
      (t--status-set name "waiting")
      (t--log "Waiting on deps for %s: %s"
              name (mapconcat #'symbol-name deps ", ")))))

(defun t--deps-installed-p (deps)
  (cl-every (lambda (dep)
              (let* ((meta (gethash dep t-package-meta))
                     (rev (plist-get meta :rev))
                     (pkg-dir (t--package-dir dep)))
                (and meta rev (t--package-up-to-date-p pkg-dir rev))))
            deps))

(defun t--activate-pending-packages ()
  (let ((pending t-package-pending-activation))
    (setq t-package-pending-activation nil)
    (dolist (spec pending)
      (let ((deps (plist-get spec :deps)))
        (if (t--deps-installed-p deps)
            (t--activate-package spec)
          (push spec t-package-pending-activation))))))

(defun t--install-finished (name)
  (t--active-remove name)
  (t--activate-pending-packages)
  (t--install-next-available)
  (when (and (= (t--active-count) 0)
             (null t-package-queue)
             (not (t--any-processes-running)))
    (setq t-package-in-progress nil)
    (t--spinner-stop)
    (unless t-package-complete-announced
      (setq t-package-complete-announced t)
      (message "t: package installs complete.")
      (run-hooks 't-package-install-complete-hook))))

(defun t--install-next-available ()
  (while (and t-package-queue
              (< (t--active-count) t-package-max-parallel))
    (t--install-package (pop t-package-queue))))

(defun t--clone-sentinel (p _event)
  (when (eq (process-status p) 'exit)
    (let* ((name (process-get p 't-name))
           (spec (process-get p 't-spec))
           (rev (process-get p 't-rev))
           (pkg-dir (process-get p 't-pkg-dir))
           (head-file (process-get p 't-head-file)))
      (if (= 0 (process-exit-status p))
          (progn
            (t--status-set name "checking out")
            (let ((checkout-proc
                   (make-process
                    :name (format "t-checkout-%s" name)
                    :filter #'t--process-filter
                    :noquery t
                    :command (list "git" "-C" pkg-dir "checkout" rev)
                    :sentinel #'t--checkout-sentinel)))
              (process-put checkout-proc 't-name name)
              (process-put checkout-proc 't-spec spec)
              (process-put checkout-proc 't-rev rev)
              (process-put checkout-proc 't-head-file head-file)))
        (t--log "Clone failed for %s" name)
        (t--status-set name "failed (clone)")
        (t--install-finished name)))))

(defun t--checkout-sentinel (p _event)
  (when (eq (process-status p) 'exit)
    (let* ((name (process-get p 't-name))
           (spec (process-get p 't-spec))
           (rev (process-get p 't-rev))
           (head-file (process-get p 't-head-file)))
      (if (= 0 (process-exit-status p))
          (progn
            (write-region rev nil head-file)
            (t--log "Installed %s at %s" name rev)
            (t--status-set name "installed")
            (t--activate-package spec))
        (t--log "Checkout failed for %s" name)
        (t--status-set name "failed (checkout)"))
      (t--install-finished name))))

(defun t--ensure-clean-dir (name pkg-dir)
  (when (file-directory-p pkg-dir)
    (condition-case err
        (delete-directory pkg-dir t)
      (error
       (t--log "Failed to remove %s: %s"
               pkg-dir (error-message-string err))
       (t--status-set name "failed (cleanup)")
       (t--install-finished name)
       t))))

(defun t--install-package (spec)
  (let* ((name (plist-get spec :name))
         (host (plist-get spec :host))
         (repo (plist-get spec :repo))
         (rev (plist-get spec :rev))
         (pkg-dir (t--package-dir name))
         (head-file (t--package-head-file pkg-dir))
         (url (t--repo-url host repo)))
    (t--active-add name)
    (t--status-set name "cloning")
    (unless (t--ensure-clean-dir name pkg-dir)
      (t--log "Installing %s from %s..." name url)
      (let ((clone-process
             (make-process
              :name (format "t-clone-%s" name)
              :filter #'t--process-filter
              :noquery t
              :command (list "git" "clone" url pkg-dir)
              :sentinel #'t--clone-sentinel)))
        (process-put clone-process 't-name name)
        (process-put clone-process 't-spec spec)
        (process-put clone-process 't-rev rev)
        (process-put clone-process 't-pkg-dir pkg-dir)
        (process-put clone-process 't-head-file head-file)))))

(defun t-install-queued-packages ()
  (interactive)
  (when (and t-package-in-progress
             (= (t--active-count) 0)
             (not (t--any-processes-running)))
    (setq t-package-in-progress nil)
    (t--spinner-stop))
  (unless t-package-in-progress
    (when (and (null t-package-queue)
               t-package-registry)
      (t-rescan-packages t))
    (if t-package-queue
        (let* ((names (mapcar (lambda (spec) (plist-get spec :name))
                              t-package-queue))
               (unique (delete-dups (copy-sequence names)))
               (count (length unique))
               (prompt (format "Install %d missing package(s): %s? "
                               count (mapconcat #'symbol-name unique ", "))))
          (when (or noninteractive
                    (yes-or-no-p prompt))
            (setq t-package-in-progress t)
            (setq t-package-complete-announced nil)
            (t--reset-queued-statuses)
            (t--display-status-and-log-buffers-exclusive)
            (t--spinner-start)
            (t--install-next-available)))
      (message "t: packages already installed."))))

(defun t-packages-buffer ()
  (interactive)
  (t--display-status-buffer))

(defun t-packages-log ()
  (interactive)
  (display-buffer (get-buffer-create t-package-log-buffer)))

(defun t-uninstall-package (name)
  (interactive
   (list
    (intern
     (completing-read
      "Uninstall package: "
      (mapcar (lambda (spec)
                (symbol-name (plist-get spec :name)))
              t-package-registry)
      nil t))))
  (let ((pkg-dir (t--package-dir name)))
    (if (not (file-directory-p pkg-dir))
        (message "t: %s is not installed." name)
      (when (yes-or-no-p (format "Delete %s? " pkg-dir))
        (condition-case err
            (progn
              (delete-directory pkg-dir t)
              (t--status-set name "removed")
              (message "t: removed %s." name))
          (error
           (message "t: failed to remove %s (%s)."
                    name (error-message-string err))))))))

(defun t-rescan-packages (&optional no-show)
  (interactive)
  (setq t-package-queue nil)
  (setq t-package-order nil)
  (setq t-package-pending-activation nil)
  (clrhash t-package-status)
  (dolist (spec t-package-registry)
    (let* ((name (plist-get spec :name))
           (pkg-dir (t--package-dir name))
           (rev (plist-get spec :rev)))
      (t--status-register name)
      (cond
       ((t--package-up-to-date-p pkg-dir rev)
        (t--status-set name "installed")
        (t--package-add-load-path name (plist-get spec :subdir)))
       ((t--package-outdated-p pkg-dir rev)
        (t--status-set name "outdated")
        (t--queue-package spec))
       (t
        (t--status-set name "queued")
        (t--queue-package spec)))))
  (unless no-show
    (t--display-status-buffer)
    (message "t: rescan complete.")))

(defun t--register-package (name host repo rev subdir use-package-form &optional deps dep-of)
  (let* ((existing (t--registry-find name))
         (existing-rev (and existing (plist-get existing :rev)))
         (existing-dep-of (and existing (plist-get existing :dep-of)))
         (rev-changed (and existing (not (string= existing-rev rev)))))
    (if (and rev-changed (or existing-dep-of dep-of))
        (progn
          (t--log "Duplicate package %s with mismatched revs: %s vs %s"
                  name existing-rev rev)
          (user-error "t: package %s declared with different revs: %s vs %s"
                      name existing-rev rev))
      (let* ((pkg-dir (t--package-dir name))
             (spec (list :name name
                         :host host
                         :repo repo
                         :rev rev
                         :subdir subdir
                         :deps deps
                         :dep-of dep-of
                         :use-package-form use-package-form)))
        (t--registry-add spec)
        (t--status-register name)
        (puthash name (list :host host :repo repo :rev rev :deps deps :dep-of dep-of)
                 t-package-meta)
        (cond
         ((t--package-up-to-date-p pkg-dir rev)
          (t--status-set name "installed")
          (t--package-add-load-path name subdir)
          (when use-package-form
            (eval (copy-tree use-package-form))))
         ((t--package-outdated-p pkg-dir rev)
          (t--status-set name "outdated")
          (t--queue-package spec))
         (t
          (t--status-set name "queued")
          (t--queue-package spec)))))))

(defmacro t-package (name host repo rev &optional subdir &rest use-package-args)
  (declare (indent defun))
  (let (deps filtered)
    (while use-package-args
      (let ((arg (pop use-package-args)))
        (if (eq arg :deps)
            (setq deps (pop use-package-args))
          (push arg filtered))))
    (setq filtered (nreverse filtered))
    (let* ((form (cons 'use-package (cons name filtered)))
           (dep-names (mapcar (lambda (dep) (car dep)) deps))
           (dep-forms
            (mapcar (lambda (dep)
                      (pcase dep
                        (`(,dname ,dhost ,drepo ,drev)
                         `(t--register-package ',dname ',dhost ,drepo ,drev nil nil nil ',name))
                        (`(,dname ,dhost ,drepo ,drev ,dsubdir)
                         `(t--register-package ',dname ',dhost ,drepo ,drev ,dsubdir nil nil ',name))
                        (_ (error "Invalid :deps entry: %S" dep))))
                    deps)))
      `(progn
         ,@dep-forms
         (t--register-package
          ',name ',host ,repo ,rev ,subdir ',form ',dep-names nil)))))

(add-hook 'emacs-startup-hook #'t-install-queued-packages)
;; / packages-diy

(add-hook 't-package-install-complete-hook
	        (lambda ()
	          (unless t--reload-in-progress
	            (let ((t--reload-in-progress t))
		            (load-file user-init-file)
		            (when (and (fboundp 'evil-mode) (not evil-mode))
                  (evil-mode 1))
		            (message "Ready, go.")))))

;;; custom
(setq custom-file null-device)

;;; server
(require 'server)
;; make server dir the same if emacs or open -a Emacs is run
;; emacsclient --socket-name $HOME/.emacs.d/server/server
(let ((dir (expand-file-name "server" user-emacs-directory)))
  (unless (file-directory-p dir)
    (make-directory dir t)
    (set-file-modes dir #o700))
  (setq server-socket-dir dir))
(let ((server-file (expand-file-name server-name server-socket-dir)))
  (unless (server-running-p server-name)
    (when (file-exists-p server-file) (ignore-errors (delete-file server-file)))
    (server-start)))

;;; y/n
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;; micro state
(defun t/micro-state (quit key fn &rest bindings)
  "Micro state that temporarily overlays a new key map, kinda like hydra.
When 'quit' is set, quits window when any other key is pressed."
  (let ((keymap (make-sparse-keymap)))
    (while key
      (keymap-set keymap key fn)
      (setq key (pop bindings)
            fn (pop bindings)))
    (lambda ()
      (interactive)
      (let ((exit (set-temporary-overlay-map
		               keymap t (lambda () (when quit (quit-window))))))
        (when quit
          (keymap-set keymap "q" (cmd! (funcall exit))))))))

;;; micro state: read
(defun t/read ()
  (interactive)
  (funcall
   (t/micro-state
    nil
    "p" 'evil-scroll-up
    "n" 'evil-scroll-down)))

;;; secrets: refresh cache
(defun t/read-first-secret (&rest args)
  (let ((secret (apply 'auth-source-pick-first-password args)))
    (when (not secret)
      (epa-file-enable)
      (auth-source-forget-all-cached))
    (apply 'auth-source-pick-first-password args)))

;;; macros
(defmacro comment (&rest _ignore) nil)
(comment (funcall (t/micro-state nil "j" 'previous-line)))

(defmacro cmd! (&rest body)
  `(lambda (&rest _args)
     (interactive)
     ,@body))

(defmacro after! (targets &rest body)
  (declare (indent 1))
  (let* ((target-list (cond
                       ((and (consp targets) (eq (car targets) 'quote))
                        (cadr targets))
                       ((listp targets) targets)
                       (t (list targets))))
         (form `(progn ,@body)))
    (dolist (target (reverse target-list) form)
      (setq form `(with-eval-after-load ',target ,form)))))

;;; default binds
(keymap-set global-map "s-a" #'mark-whole-buffer)
(keymap-set global-map "s-c" #'kill-ring-save)
(keymap-set global-map "s-j" #'next-buffer)
(keymap-set global-map "s-k" #'previous-buffer)
(keymap-set global-map "s-n" 'make-frame)
(keymap-set global-map "s-s" #'save-buffer)
(keymap-set global-map "s-v" #'evil-paste-after)
(keymap-set global-map "s-d" (cmd! (split-window-horizontally) (evil-window-right 1)))
(keymap-set global-map "s-D" (cmd! (split-window-vertically) (evil-window-down 1)))
(keymap-set global-map "s-q" #'evil-save-and-quit)

;;; t-sidebar
(defvar t-sidebar-buffer-prefix ":")

(defun t--sidebar-width ()
  (let* ((fw (frame-width))
         (div (if (> fw 250) 4 3)))
    (/ fw div)))

(defun t--resize-sidebar (win)
  (when (window-live-p win)
    (let ((desired (t--sidebar-width)))
      (when (integerp desired)
        (let ((delta (- desired (window-width win))))
          (when (/= delta 0)
            (condition-case _err
                (window-resize win delta t)
              (error nil))))))))

(defun t--display-sidebar (buffer alist)
  (let ((win (display-buffer-in-side-window
              buffer
              (append alist
                      '((side . left)
                        (slot . 0)
                        (window-parameters . ((no-delete-other-windows . t))))))))
    (when (window-live-p win)
      (with-selected-window win
        (set-window-dedicated-p win t)
        (t--resize-sidebar win)
        (setq-local window-size-fixed 'width)))
    win))

(add-to-list 'display-buffer-alist
             `(,(concat "^" (regexp-quote t-sidebar-buffer-prefix))
               . (t--display-sidebar)))

(defun t-toggle-sidebar ()
  (interactive)
  (let* ((sidebar-project (t/project-root))
         (sidebar-name (concat t-sidebar-buffer-prefix sidebar-project))
         (sidebar-buffer (get-buffer sidebar-name))
         (sidebar-displayed (and sidebar-buffer (get-buffer-window sidebar-buffer))))
    (if sidebar-displayed
        (delete-window (get-buffer-window sidebar-buffer))
      (when (not sidebar-buffer)
        (with-current-buffer (dired-noselect sidebar-project)
          ;; unadvertise buffer so dired does not consider it on subsequent dired-jump
          (dired-unadvertise (dired-current-directory))
          (rename-buffer sidebar-name)
          (dired-hide-details-mode)))
      (progn
        (pop-to-buffer sidebar-name)
        (set-window-dedicated-p (selected-window) t)
        (set-window-parameter (selected-window) 'no-delete-other-windows t)))))

;;; dired, no hidden files
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)
            (setq-local dired-omit-files "\\`\\..+\\'")))
;; TODO better way
(setq insert-directory-program "/etc/profiles/per-user/torgeir/bin/ls"
      dired-listing-switches "-al --group-directories-first")

(t-package dired-subtree gh "Fuco1/dired-hacks" "de9336f" nil
  :deps ((dash gh "magnars/dash.el" "d3a84021"))
  :commands (dired-subtree-toggle dired-subtree--dired-line-is-directory-or-link-p))

(t-package s gh "magnars/s.el" "dda84d3" nil)

(defun t/prefix-arg-universal? ()
  "Check if the function was called with the c-u universal prefix."
  (equal '(4) current-prefix-arg))

(defun t/dired-subtree-tab ()
  (interactive)
  (unless (featurep 'dired-subtree)
    (require 'dired-subtree))
  (cond
   ((and (t/prefix-arg-universal?)
         (dired-subtree--is-expanded-p)) (t/dired-close-recursively))
   ((t/prefix-arg-universal?) (t/dired-open-recursively))
   (t (t/dired-subtree-toggle))))

(defun t/dired-subtree-toggle ()
  (if (eq major-mode 'dired-sidebar-mode)
      (dired-sidebar-subtree-toggle)
    (dired-subtree-toggle)))

(defun t/dired-close-recursively ()
  "Close all directories starting with this directory's path."
  (interactive)
  (let ((path (dired-get-filename)))
    (save-excursion
      (while
          (and
           (not (eq (point) (save-excursion (end-of-buffer) (point))))
           (s-starts-with? path (dired-get-filename)))
        (when (dired-subtree--is-expanded-p)
          (t/dired-subtree-toggle))
        (forward-line)))))

(defun t/dired-open-recursively ()
  "Open all directories starting with this directory's path."
  (interactive)
  (let ((path (dired-get-filename)))
    (save-excursion
      (when (dired-subtree--is-expanded-p) (t/dired-subtree-toggle))
      (t/dired-subtree-toggle)
      (forward-line)
      (t/dired-show-recursively-0 path))))

(defun t/project-root ()
  "Get project root without throwing."
  (interactive)
  (or (string-trim-right (shell-command-to-string "git rev-parse --show-toplevel") "\n")
      (expand-file-name default-directory)))

(defun t/dired-ignored? ()
  "File under cursor is ignored by projectile. Only checks file-name-base."
  (interactive)
  (-any?
   (lambda (d) (s-matches? d (file-name-base (dired-get-filename nil t))))
   (and (boundp 'projectile-globally-ignored-directories)
	      projectile-globally-ignored-directories)))

(defun t/dired-show-recursively-0 (path)
  "Recursively opens all directories for this path."
  (interactive)
  (when (not (eq (point) (save-excursion (end-of-buffer) (point))))
    (when (and
           (not (t/dired-ignored?))
           (s-starts-with? path (dired-get-filename nil t))
           (not (s-starts-with? "." (file-name-base (dired-get-filename nil t))))
           )
      (when (dired-subtree--is-expanded-p) (t/dired-subtree-toggle))
      (t/dired-subtree-toggle))
    (forward-line)
    (t/dired-show-recursively-0 path)))

(defun t/dired-locate-path (path)
  "Locate PATH in a dired listing."
  (unless (featurep 'dired-subtree)
    (require 'dired-subtree))
  (beginning-of-buffer)
  (forward-line)
  (dolist (el path)
    (search-forward el nil t)
    (when (not (dired-subtree--is-expanded-p))
      (dired-subtree-toggle)))
  (dired-move-to-filename))

(defun t/dired-collapse ()
  "Collapse all expanded directories in dired listing."
  (beginning-of-buffer)
  (while (not (eobp))
    (forward-line)
    (when (dired-subtree--is-expanded-p)
      (dired-subtree-toggle)))
  (beginning-of-buffer))

(defun t/dired-locate ()
  "Locate the visited file in dired or t-sidebar."
  (interactive)
  (let* ((path (s-replace (t/project-root) "" (or (buffer-file-name) "")))
         (path (s-split "/" path))
         (path (remove "" path)))
    (let* ((sidebar (concat ":" (t/project-root))))
      (if (get-buffer sidebar)
          (pop-to-buffer sidebar)
        (t-toggle-sidebar)))
    (when (t/prefix-arg-universal?)
      (t/dired-collapse))
    (t/dired-locate-path path)))

;;; init tweaks
;; faster startup: avoid initializing a heavier major mode in the first buffer.
(setq initial-major-mode 'fundamental-mode
      inhibit-startup-screen t
      initial-scratch-message nil)

;; minibuffer prompt should be read-only and cursor-intangible.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; smoother scrolling with fewer expensive recalculations.
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 20)
(setq auto-window-vscroll nil)
(setq scroll-preserve-screen-position t)

;; reduce lag while typing in large buffers.
(setq redisplay-skip-fontification-on-input t)

;; no backups, lockfiles, or autosave files.
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;;; leader key
(defvar t-leader "SPC")
(defvar t-leader-alt "M-SPC")
(defvar t-leader-map (make-sparse-keymap))

(defvar t-leader-g-map (make-sparse-keymap) "git")
(keymap-set t-leader-map "g" t-leader-g-map)

;;; fonts
(setq t-font-height 200)
(set-face-attribute 'default nil
                    :family "Iosevka Nerd Font Mono"
                    :height t-font-height)
(set-face-attribute 'variable-pitch nil
                    :family "IosevkaTerm Nerd Font Propo"
                    :height (- t-font-height 50)
                    :weight 'ultra-light
                    :slant 'normal)

(keymap-set global-map "C-+" (cmd! (text-scale-set
				                            (1+ text-scale-mode-amount))))
(keymap-set global-map "C--" (cmd! (text-scale-set
				                            (1- text-scale-mode-amount))))
(keymap-set global-map "C-0" (cmd! (text-scale-set 0)))
(defun t/adjust-font (fn inc)
  (interactive)
  (let ((h (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (funcall fn h inc))))
(defun t/reset-font ()
  (interactive)
  (set-face-attribute 'default nil :height t-font-height))
(keymap-set global-map "s-<kp-add>" (cmd! (t/adjust-font '+ 10)))
(keymap-set global-map "s-+" (cmd! (t/adjust-font '+ 10)))
(keymap-set global-map "s--" (cmd! (t/adjust-font '- 10)))
(keymap-set global-map "s-<wheel-down>" (cmd! (t/adjust-font '- 10)))
(keymap-set global-map "s-<wheel-up>" (cmd! (t/adjust-font '+ 10)))
(keymap-set global-map "s-<mouse-2>" #'t/reset-font)
(keymap-set global-map "s-0" #'t/reset-font)

;;; mouse nav
(keymap-set global-map "<mouse-8>" 'previous-buffer)
(keymap-set global-map "<mouse-9>" 'next-buffer)

;;; general
(keymap-set t-leader-map "," (cmd!
			                        (if (t/prefix-arg-universal?)
				                          (call-interactively 'consult-buffer-other-window)
				                        (call-interactively 'consult-buffer)
				                        )))
(keymap-set t-leader-map "RET" #'consult-bookmark)
(keymap-set t-leader-map "u" #'universal-argument)
(keymap-set t-leader-map t-leader #'find-file)
(let ((maps (list minibuffer-local-map
		              minibuffer-local-ns-map
		              minibuffer-local-completion-map
		              minibuffer-local-must-match-map
		              minibuffer-local-isearch-map)))
  (dolist (map maps)
    (keymap-set map t-leader-alt t-leader-map)))

(setq display-line-numbers-type 'relative)

(setq make-backup-files nil
      auto-save-default nil)

;;; diy smartparens
(defvar t--pairs-alist nil)

(defun t--apply-pairs (pairs)
  (electric-pair-local-mode 1)
  (let ((seen nil)
	      (combined nil))
    (dolist (pair (append (default-value 'electric-pair-pairs) pairs))
      (unless (member pair seen)
	      (push pair seen)
	      (push pair combined)))
    (setq-local electric-pair-pairs (nreverse combined)))
  (setq-local electric-pair-text-pairs electric-pair-pairs)
  (setq-local electric-pair-inhibit-predicate
	            (lambda (c)
		            (and (not (assoc c pairs))
		                 (funcall #'electric-pair-default-inhibit c)))))

(defun t--apply-pairs-for-mode ()
  (let ((pairs (alist-get major-mode t--pairs-alist)))
    (when pairs
      (t--apply-pairs pairs))))

(defun t/set-pairs (mode pairs)
  (setf (alist-get mode t--pairs-alist) pairs)
  (add-hook (intern (format "%s-hook" mode)) #'t--apply-pairs-for-mode))

;;; toggles
(keymap-set t-leader-map "t d" #'toggle-debug-on-error)
(keymap-set t-leader-map "t e" #'global-emojify-mode)
(keymap-set t-leader-map "t i" (defun t/toggle-images ()
				                         (interactive)
				                         (when (equal major-mode 'eww-mode)
				                           (eww-toggle-images))))
(keymap-set t-leader-map "t b" #'tool-bar-mode)
(keymap-set t-leader-map "t l" #'display-line-numbers-mode)
(keymap-set t-leader-map "t L" (cmd!
				                        (display-line-numbers-mode -1)
				                        (setq display-line-numbers-type
				                              (if (eq t display-line-numbers-type)
					                                'relative
					                              t))
				                        (display-line-numbers-mode 1)))
(keymap-set t-leader-map "t m" #'menu-bar-mode)
(keymap-set t-leader-map "t u" #'toggle-truncate-lines)
(keymap-set t-leader-map "t v" #'visual-line-mode)
(keymap-set t-leader-map "t w" #'whitespace-mode)
;;(keymap-set t-leader-map "t t" #'doric-themes-toggle)
(keymap-set t-leader-map "t t" #'doric-themes-rotate)
(keymap-set t-leader-map "t T" #'t/transparency)
(keymap-set t-leader-map "t r" (cmd!
                                (buffer-face-mode 'toggle)
                                (when buffer-face-mode (t/read))))

;;; quit
(keymap-set t-leader-map "q r" #'restart-emacs)

;;; buffers
(keymap-set t-leader-map "b b" #'consult-buffer)
(keymap-set t-leader-map "b B" #'persp-switch-to-buffer)
(keymap-set t-leader-map "b i" #'list-buffers)
(keymap-set t-leader-map "b d" #'kill-current-buffer)
(keymap-set t-leader-map "b n" #'evil-buffer-new)
(keymap-set t-leader-map "b O" #'persp-kill-other-buffers)

;;; compile
(keymap-set t-leader-map "c c" #'compile)

;;; magit
(t-package magit gh "magit/magit" "b9f19ba" nil
  :commands (magit-log
	           magit-status
	           magit-file-stage
	           magit-diff-dwim
	           magit-push
	           magit-commit
	           magit-commit-create
	           magit-commit-amend
	           magit-commit-instant-fixup)
  :deps ((compat   gh "emacs-compat/compat" "38df650")
	       (cond-let gh "tarsius/cond-let" "8bf87d4")
	       (llama    gh "tarsius/llama" "d430d48")
	       (with-editor gh "magit/with-editor" "64211dc")
	       (transient gh "magit/transient" "7131bec"))
  :init
  (when is-mac
    (setq magit-git-executable "/etc/profiles/per-user/torgeir/bin/git"))
  :config
  (add-hook 'magit-revision-mode-hook 'toggle-truncate-lines)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (dolist (magit-map (list
		                  magit-diff-mode-map
		                  magit-log-mode-map
		                  magit-status-mode-map))
    (keymap-set magit-map t-leader t-leader-map)))

;; magit binds
(keymap-set t-leader-g-map "g" #'magit-status)
(keymap-set t-leader-g-map "l" #'magit-log)
(keymap-set t-leader-g-map "d" #'magit-diff-dwim)
(keymap-set t-leader-g-map "e" #'magit-ediff-dwim)
(keymap-set t-leader-g-map "P" #'magit-push)
(keymap-set t-leader-g-map "c a" #'magit-commit-amend)
(keymap-set t-leader-g-map "c c" #'magit-commit-create)
(keymap-set t-leader-g-map "c F" #'magit-commit-instant-fixup)

;; hunk binds
(keymap-set t-leader-g-map "h" (t/micro-state
				                        nil
				                        "a" 'magit-commit-amend
				                        "c" 'magit-commit-create
				                        "f" 'magit-commit-instant-fixup
				                        "?" 'diff-hl-revert-hunk
 				                        "r" 'diff-hl-revert-hunk
				                        "s" 'diff-hl-stage-current-hunk
				                        "p" 'diff-hl-previous-hunk
				                        "n" 'diff-hl-next-hunk
				                        "k" 'diff-hl-previous-hunk
				                        "j" 'diff-hl-next-hunk
				                        ))
(keymap-set t-leader-g-map "S" #'magit-file-stage)
(keymap-set t-leader-g-map "U" #'diff-hl-unstage-file)
(keymap-set t-leader-g-map "r" #'diff-hl-revert-hunk)
(keymap-set t-leader-g-map "n" #'diff-hl-next-hunk)
(keymap-set t-leader-g-map "p" #'diff-hl-previous-hunk)
(keymap-set t-leader-g-map "s" #'diff-hl-stage-current-hunk)

;;; search
(defun t/consult-line-dwim (&optional ignore init)
  "'consult-line', also in 'vterm-mode', without returning to prompt when done."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (vterm-copy-mode 1))
  (consult-line init))
(defun t/consult-with-region (fn)
  "Run consult 'fn' with selected region as the query."
  (lambda (arg)
    (interactive "P")
    (let ((region (and (region-active-p)
		                   (buffer-substring-no-properties
			                  (region-beginning) (region-end)))))
      (evil-exit-visual-state)
      (let ((this-command fn)
            (real-this-command fn))
        (funcall fn arg region)))))
(keymap-set t-leader-map "." (t/consult-with-region 'consult-find))
(keymap-set t-leader-map "s s" (t/consult-with-region 't/consult-line-dwim))
(keymap-set t-leader-map "s S" (t/consult-with-region 'consult-line-multi))
(keymap-set t-leader-map "s p" (t/consult-with-region 'consult-ripgrep))
(keymap-set t-leader-map "s g" (t/consult-with-region 'consult-git-grep))
(keymap-set t-leader-map "s G" (t/consult-with-region 'consult-grep))
(keymap-set t-leader-map "s i" 'consult-imenu)
(keymap-set t-leader-map "s d" (cmd! (dired (read-directory-name "Ripgrep dir: " default-directory nil t))))
(keymap-set t-leader-map "s M" #'evil-show-marks)
(keymap-set t-leader-map "s R" #'evil-show-registers)
(keymap-set t-leader-map "s e" #'emoji-search)

;;; files
(keymap-set t-leader-map "f f" #'find-file)
(keymap-set t-leader-map "f D" (cmd! (dired (read-directory-name "Dired: "))))
(keymap-set t-leader-map "f r" #'recentf)
(keymap-set t-leader-map "f l" #'t-toggle-sidebar)
(keymap-set t-leader-map "f L" 't/dired-locate)

(keymap-set t-leader-map "f P"
            (cmd!
             (let ((default-directory user-emacs-directory))
               (call-interactively #'consult-find))))

;;; avy
(t-package avy gh "abo-abo/avy" "933d1f3" nil
  :deps ((cl-lib sav "emacs/elpa.git" "790948a"))
  :commands (avy-goto-char-2
	           avy-goto-line
	           avy-move-line
	           avy-goto-word-or-subword-1)
  :init
  (keymap-set t-leader-map "j c" #'avy-goto-char-2)
  (keymap-set t-leader-map "j l" #'avy-goto-line)
  (keymap-set t-leader-map "j m" #'avy-move-line)
  (keymap-set t-leader-map "j w" #'avy-goto-word-or-subword-1)
  (setq avy-keys '(?j ?f ?d ?k ?s ?a)
	      avy-timeout-seconds 0.2
	      ;;avy-all-windows 'all-frames
	      avy-all-windows nil
	      avy-case-fold-search nil
	      avy-highlight-first t
	      avy-style 'at-full
	      avy-background t)
  (defun t/setup-avy ()
    (interactive)
    (after! avy
      (let* ((bb (face-background 'default nil t))
	           (b (face-background 'default nil t))
	           (f "DeepPink1"))
	      (set-face-attribute 'avy-background-face nil :foreground "#657")
	      (set-face-attribute 'avy-lead-face   nil :background b :foreground f :weight 'bold)
	      (set-face-attribute 'avy-lead-face-0 nil :background b :foreground f :weight 'bold)
	      (set-face-attribute 'avy-lead-face-1 nil :background b :foreground f :weight 'bold)
	      (set-face-attribute 'avy-lead-face-2 nil :background b :foreground f :weight 'bold)
	      t)))
  (add-hook 'after-change-major-mode-hook 't/setup-avy))

;;; jump to links
(t-package link-hint gh "noctuid/link-hint.el" "8fda5dc" nil
  :deps ((avy gh "abo-abo/avy" "933d1f3"))
  :commands (link-hint-open-link
             link-hint-copy-link)
  :init
  (keymap-set t-leader-map "s l" (cmd!
                                  (if (t/prefix-arg-universal?)
                                      (call-interactively 'link-hint-copy-link)
                                    (call-interactively 'link-hint-open-link))))
  (keymap-set t-leader-map "s L" #'ffap-menu))

;;; eval
(keymap-set t-leader-map "m e b" #'eval-buffer)

;;; project
(defun t--project-dir-name (dir)
  (file-name-nondirectory (directory-file-name dir)))
(defun t--project-persp-name (project)
  (t--project-dir-name (project-root project)))
(defun t--persp-switch-to-project (&optional project)
  (when-let ((proj (or project (project-current))))
    (persp-switch (t--project-persp-name proj))))
(defun t-project-switch-find-file ()
  (interactive)
  (require 'project)
  (let* ((dir (project-prompt-project-dir))
         (default-directory (file-name-as-directory dir))
         (proj (project-current nil default-directory)))
    (persp-switch (t--project-dir-name default-directory))
    (when proj
      (project-remember-project proj))
    (project-find-file)))

;;; describe
(keymap-set t-leader-map "h a" #'apropos)
(keymap-set t-leader-map "h b" #'describe-bindings)
(keymap-set t-leader-map "h c" #'describe-command)
(keymap-set t-leader-map "h C" #'describe-char)
(keymap-set t-leader-map "h e" #'view-echo-area-messages)
(keymap-set t-leader-map "h f" #'describe-function)
(keymap-set t-leader-map "h i" (cmd! (require 'consult-info) (consult-info)))
(keymap-set t-leader-map "h h" #'consult-man)
(keymap-set t-leader-map "h F" #'describe-face)
(keymap-set t-leader-map "h k" #'describe-key)
(keymap-set t-leader-map "h m" #'describe-mode)
(keymap-set t-leader-map "h M" #'describe-keymap)
(keymap-set t-leader-map "h p" #'describe-package)
(keymap-set t-leader-map "h s" #'describe-symbol)
(keymap-set t-leader-map "h v" #'describe-variable)
(keymap-set t-leader-map "h r r" (cmd! (load-file (expand-file-name "init.el" user-emacs-directory))))

;;; code
(keymap-set t-leader-map "c d" #'xref-find-definitions)
(keymap-set t-leader-map "p p" #'t-project-switch-find-file)

;;; windows
(keymap-set t-leader-map "w d" #'delete-window)
(keymap-set t-leader-map "w m m" #'delete-other-windows)
(keymap-set t-leader-map "w t" #'evil-window-rotate-downwards)

;;; window nav
(keymap-set t-leader-map "w l" #'evil-window-right)
(keymap-set t-leader-map "w h" #'evil-window-left)
(keymap-set t-leader-map "w j" #'evil-window-down)
(keymap-set t-leader-map "w k" #'evil-window-up)
(keymap-set t-leader-map "w L" #'evil-window-move-far-right)
(keymap-set t-leader-map "w H" #'evil-window-move-far-left)
(keymap-set t-leader-map "w K" #'evil-window-move-very-top)
(keymap-set t-leader-map "w J" #'evil-window-move-very-bottom)

(keymap-set global-map "M-s-<up>" #'evil-window-up)
(keymap-set global-map "M-s-<right>" #'evil-window-right)
(keymap-set global-map "M-s-<left>" #'evil-window-left)
(keymap-set global-map "M-s-<down>" #'evil-window-down)

(keymap-set t-leader-map "w M"
	          (t/micro-state nil
			                     "<left>" (cmd! (cond
					                                 ((and (boundp 'olivetti-mode) olivetti-mode)
					                                  (olivetti-shrink))))
			                     "<right>" (cmd! (cond
					                                  ((and (boundp 'olivetti-mode) olivetti-mode)
					                                   (olivetti-expand))))))
(keymap-set t-leader-map "w s"
	          (t/micro-state
	           nil
	           "<left>" (cmd! (cond
			                       ((and (window-in-direction 'right) (window-in-direction 'left))
			                        (evil-resize-window (- (window-width) 8) t))
			                       ((window-in-direction 'left) (evil-resize-window (+ (window-width) 8) t))
			                       ((window-in-direction 'right) (evil-resize-window (- (window-width) 8) t))
			                       (t (execute-kbd-macro "h"))))
	           "<right>" (cmd! (cond
			                        ((and (window-in-direction 'right) (window-in-direction 'left))
			                         (evil-resize-window (+ (window-width) 8) t))
			                        ((window-in-direction 'right) (evil-resize-window (+ (window-width) 8) t))
			                        ((window-in-direction 'left) (evil-resize-window (- (window-width) 8) t))
			                        (t (execute-kbd-macro "l"))))
	           "<up>" (cmd! (cond
			                     ((and (window-in-direction 'up) (window-in-direction 'down))
			                      (evil-resize-window (+ (window-height) 4)))
			                     ((window-in-direction 'down) (evil-resize-window (- (window-height) 4)))
			                     ((window-in-direction 'up) (evil-resize-window (+ (window-height) 4)))
			                     (t (execute-kbd-macro "k"))))
	           "<down>" (cmd! (cond
			                       ((and (window-in-direction 'up) (window-in-direction 'down))
			                        (evil-resize-window (- (window-height) 4)))
			                       ((window-in-direction 'up) (evil-resize-window (- (window-height) 4)))
			                       ((window-in-direction 'down) (evil-resize-window (+ (window-height) 4)))
			                       (t (execute-kbd-macro "j"))))))

;;; undo
(t-package undo-fu gh "emacsmirror/undo-fu" "b4ce5ed" nil
  :init
  (setq evil-undo-system 'undo-fu))

;;; undo tree
(t-package vundo gh "casouri/vundo" "e0af8c5" nil)

;;; evil
;; Must be set before loading evil or evil-collection.
(setq evil-want-keybinding nil)
(t-package evil gh "emacs-evil/evil" "729d9a5" nil
  :init
  (setq evil-want-integration t
	      evil-want-keybinding nil
	      evil-want-C-u-scroll t
	      evil-split-window-right t
	      evil-split-window-below t
	      evil-move-beyond-eol t
	      evil-search-module 'evil-search)
  :hook (after-init . evil-mode)
  :config
  (defun t--set-evil-cursors (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (let ((normal-color (or (face-foreground 'cursor nil t)
			                        (frame-parameter nil 'cursor-color)
			                        (face-foreground 'default nil t))))
	      (setq evil-emacs-state-cursor '(box "orange")
	            evil-normal-state-cursor `(box ,normal-color))
	      (evil-refresh-cursor))))
  (add-hook 'after-load-theme-hook #'t--set-evil-cursors)
  (add-hook 'after-make-frame-functions #'t--set-evil-cursors)
  (add-hook 'after-init-hook #'t--set-evil-cursors)
  (t--set-evil-cursors)
  (defun t-move-line-up ()
    "Move the current line up by one."
    (interactive)
    (transpose-lines 1)
    (forward-line -2))
  (defun t-move-line-down ()
    "Move the current line down by one."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1))
  ;; leader
  (keymap-set evil-visual-state-map t-leader t-leader-map)
  (keymap-set evil-motion-state-map t-leader t-leader-map)
  (keymap-set evil-normal-state-map t-leader t-leader-map)
  (keymap-set evil-motion-state-map t-leader-alt t-leader-map)
  (keymap-set evil-normal-state-map t-leader-alt t-leader-map)
  (keymap-set evil-visual-state-map t-leader-alt t-leader-map)
  (keymap-set evil-insert-state-map t-leader-alt t-leader-map)
  (keymap-set evil-emacs-state-map  t-leader-alt t-leader-map)
  ;; scroll
  (keymap-set evil-normal-state-map "C-a" #'evil-beginning-of-line)
  (keymap-set evil-motion-state-map "C-a" #'evil-beginning-of-line)
  (keymap-set evil-insert-state-map "C-a" #'evil-beginning-of-line)
  (keymap-set evil-normal-state-map "C-e" #'evil-end-of-line)
  (keymap-set evil-motion-state-map "C-e" #'evil-end-of-line)
  (keymap-set evil-insert-state-map "C-e" #'evil-end-of-line)
  (keymap-set evil-normal-state-map "C-k" #'kill-line)
  (keymap-set evil-insert-state-map "C-k" #'kill-line)
  (keymap-set evil-normal-state-map "M-<up>" #'t-move-line-up)
  (keymap-set evil-normal-state-map "M-<down>" #'t-move-line-down)
  
  (after! evil
    (evil-define-key 'normal Buffer-menu-mode-map (kbd "RET") #'Buffer-menu-select)
    (evil-define-key 'motion Buffer-menu-mode-map (kbd "RET") #'Buffer-menu-select)))

;;; evil-collection
(t-package evil-collection gh "emacs-evil/evil-collection" "7680834" nil
  :deps ((annalist gh "noctuid/annalist.el" "e1ef5da")
         (evil gh "emacs-evil/evil" "729d9a5"))
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;;; info mode
(after! (evil info)
  (evil-define-key 'normal Info-mode-map (kbd t-leader) t-leader-map)
  (evil-define-key 'motion Info-mode-map (kbd t-leader) t-leader-map)
  (evil-define-key 'normal Info-mode-map (kbd t-leader-alt) t-leader-map)
  (evil-define-key 'motion Info-mode-map (kbd t-leader-alt) t-leader-map)
  (evil-define-key 'normal Info-mode-map (kbd "H") #'Info-history-back)
  (evil-define-key 'normal Info-mode-map (kbd "L") #'Info-history-forward)
  (keymap-set Info-mode-map "<mouse-8>" 'Info-history-back)
  (keymap-set Info-mode-map "<mouse-9>" 'Info-history-forward)
  (unbind-key (kbd "h") 'Info-mode-map)
  (unbind-key (kbd "l") 'Info-mode-map))

;;; help-mode leader
(after! evil
  (evil-define-key 'normal help-mode-map (kbd t-leader) t-leader-map)
  (evil-define-key 'motion help-mode-map (kbd t-leader) t-leader-map)
  (evil-define-key 'normal help-mode-map (kbd t-leader-alt) t-leader-map)
  (evil-define-key 'motion help-mode-map (kbd t-leader-alt) t-leader-map))

(after! (evil image-mode)
  (evil-define-key 'normal image-mode-map (kbd t-leader) t-leader-map)
  (evil-define-key 'motion image-mode-map (kbd t-leader) t-leader-map)
  (evil-define-key 'normal image-mode-map (kbd t-leader-alt) t-leader-map)
  (evil-define-key 'motion image-mode-map (kbd t-leader-alt) t-leader-map))

;;; help-mode
(after! (evil evil-collection)
  (evil-define-key 'normal help-mode-map (kbd "H") #'help-go-back)
  (evil-define-key 'normal help-mode-map (kbd "L") #'help-go-forward))

;;; debugger-mode
(after! (evil debug)
  (evil-define-key 'normal debugger-mode-map (kbd t-leader) t-leader-map)
  (evil-define-key 'normal debugger-mode-map (kbd t-leader-alt) t-leader-map)
  (evil-define-key 'motion debugger-mode-map (kbd t-leader) t-leader-map)
  (evil-define-key 'motion debugger-mode-map (kbd t-leader-alt) t-leader-map))

;;; evil-commentary
(t-package evil-commentary gh "linktohack/evil-commentary" "c5945f2" nil
  :deps ((evil gh "emacs-evil/evil" "729d9a5"))
  :config
  (evil-commentary-mode))

;;; TODO
(t-package hl-todo gh "tarsius/hl-todo" "9540fc4" nil
  :config
  (global-hl-todo-mode))

;;; theme synced persp face
(defun t/sync-persp-face (&optional frame)
  "Make selected workspace font fit the theme."
  (when (facep 'persp-selected-face)
    (with-selected-frame (or frame (selected-frame))
      (set-face-attribute 'persp-selected-face nil
			                    :inherit 'mode-line
			                    :foreground (face-attribute 'mode-line :foreground nil t)
			                    :background (face-attribute 'mode-line :background nil t)
			                    ))))
(add-hook 'after-load-theme-hook #'t/sync-persp-face)
(add-hook 'doric-themes-after-load-theme-hook #'t/sync-persp-face)
(add-hook 'after-make-frame-functions #'t/sync-persp-face)
(add-hook 'after-init-hook #'t/sync-persp-face)
(add-hook 'persp-mode-hook #'t/sync-persp-face)

;;; perspectives
(t-package perspective gh "nex3/perspective-el" "64ef5ea" nil
  :init
  (setq persp-state-default-file
	      (expand-file-name "perspective-state.el" user-emacs-directory)
	      persp-mode-prefix-key nil
	      persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode 1)
  (t/sync-persp-face)
  (keymap-set t-leader-map "-" #'persp-switch)
  (keymap-set t-leader-map "TAB" 'perspective-map)
  (after! perspective
    (keymap-set perspective-map "k" #'persp-kill)
    (keymap-set perspective-map "s" #'persp-state-save)
    (keymap-set perspective-map "l" #'persp-state-load))
  (after! project
    (add-hook 'project-switch-project-hook #'t--persp-switch-to-project)))

;;; themes
(t-package doric-themes gh "protesilaos/doric-themes" "86a3b91" nil
  :init
  (setq doric-themes-to-toggle '(doric-fire doric-water)
	      doric-themes-to-rotate '(doric-fire doric-plum doric-water doric-siren))
  :config
  (doric-themes-select 'doric-plum))

;;; which-key
(t-package which-key sav "emacs/elpa.git" "ac5afbe" nil
  :init
  (which-key-mode))

;;; writing
(t-package olivetti gh "rnkn/olivetti" "845eb7a" nil
  :commands (olivetti-mode)
  :init
  (keymap-set t-leader-map "r o" #'olivetti-mode))

;;; vterm
(t-package vterm gh "akermu/emacs-libvterm" "a01a289" nil
  :commands (vterm)
  :init
  (keymap-set t-leader-map "o t" #'vterm)
  :config
  (setq vterm-shell (or (executable-find "zsh") "/bin/zsh" "/bin/bash"))
  (defun t-vterm-ctrl-d ()
    (interactive)
    (if (and (vterm--at-prompt-p)
             (= (point) (point-max)))
        (if (window-live-p (selected-window))
            (kill-buffer-and-window)
          (kill-buffer))
      (vterm-send-key "d" nil nil t)))
  (defun t-vterm-copy-mode-insert ()
    (interactive)
    (vterm-copy-mode -1)
    (vterm-reset-cursor-point)
    (evil-insert-state))
  ;; leader
  (keymap-set vterm-copy-mode-map "i" #'t-vterm-copy-mode-insert)
  (after! evil
    (evil-define-key 'insert vterm-mode-map (kbd "C-d") #'t-vterm-ctrl-d)
    ;; exit from e.g. fzf without going to normal mode, like esc
    (evil-define-key 'insert vterm-mode-map (kbd "M-<escape>") #'vterm-send-escape)
    (evil-define-key 'insert vterm-mode-map (kbd "S-<up>") (cmd! (vterm-send-key "<prior>" nil nil nil)))
    (evil-define-key 'insert vterm-mode-map (kbd "S-<down>") (cmd! (vterm-send-key "<next>" nil nil nil))))
  (defun t/vterm-project (arg)
    (interactive "P")
    (let (;;(default-directory (t/project-root)) ; force init dir to root
          (vterm-buffer-name (format "*vterm:%s*" (t/project-root))))
      (vterm arg)))
  (dolist (map (list evil-motion-state-map
                     evil-normal-state-map))
    (keymap-set map "s-<return>" #'t/vterm-project)))

;;; vterm: dired, magit etc follows terminal dir
(after! vterm
  ;; https://github.com/akermu/emacs-libvterm#how-can-i-get-the-directory-tracking-in-a-more-understandable-way
  ;; see dotfiles/source/functions
  (add-to-list
   'vterm-eval-cmds
   '("update-pwd" (lambda (path) (setq default-directory path))))

  (add-to-list
   'vterm-eval-cmds
   '("magit-diff" (lambda (path)
                    (let ((default-directory path))
                      (call-interactively' magit-diff)))))

  (add-to-list
   'vterm-eval-cmds
   '("magit-log" (lambda (path)
                   (let ((default-directory path))
                     (call-interactively' magit-log)))))

  (add-to-list
   'vterm-eval-cmds
   '("magit-status" (lambda (path)
                      (let ((default-directory path))
                        (call-interactively' magit-status))))))

;;; agent-shell
(t-package agent-shell gh "xenodium/agent-shell" "b3e556c" nil
  :deps ((acp gh "xenodium/acp.el" "f7e20ce")
	       (shell-maker gh "xenodium/shell-maker" "8c64f0b"))
  :commands (agent-shell)
  :init
  (after! agent-shell
    (setq agent-shell-openai-authentication
	        (agent-shell-openai-make-authentication
	         :codex-api-key
	         (lambda ()
	           (t/read-first-secret :host "api.openai.com")))))
  (setq agent-shell-header-style 'text
	      agent-shell-session-strategy 'prompt
	      agent-shell-screenshot-command (if is-mac nil '("grimshot" "save" "area")))
  (defun t-shell-maker-input-empty-p ()
    "Return non-nil when pending comint input is empty."
    (if-let ((proc (get-buffer-process (current-buffer))))
	      (let ((input (buffer-substring-no-properties
		                  (marker-position (process-mark proc))
		                  (point-max))))
	        (string-empty-p (string-trim input)))
      t))
  (defun t-shell-maker-ctrl-d ()
    (interactive)
    (if (and (eobp)
	           (or (not (fboundp 'shell-maker-point-at-last-prompt-p))
		             (shell-maker-point-at-last-prompt-p))
	           (t-shell-maker-input-empty-p))
	      (if (window-live-p (selected-window))
	          (kill-buffer-and-window)
	        (kill-buffer))
      (if (fboundp 'comint-delchar-or-maybe-eof)
	        (comint-delchar-or-maybe-eof 1)
	      (delete-char 1))))
  (defun t/agent-shell--recenter (&rest _)
    "Recenter comint buffers to a fixed offset after submit."
    (when (derived-mode-p 'comint-mode)
      (when-let ((win (get-buffer-window (current-buffer) t)))
        (with-selected-window win
          (recenter-top-bottom 5)))))
  (advice-add 'shell-maker-submit :after #'t/agent-shell--recenter)
  (keymap-set t-leader-map "a a" #'agent-shell)
  (keymap-set t-leader-map "a i" #'agent-shell)
  (keymap-set t-leader-map "a c" #'agent-shell)
  (after! evil
    (evil-define-key 'insert agent-shell-mode-map (kbd "C-k") #'kill-line)
    (evil-define-key 'insert agent-shell-mode-map (kbd "C-d") #'t-shell-maker-ctrl-d)))

;;; chatgpt-shell
(t-package chatgpt-shell gh "xenodium/chatgpt-shell" "cbad6ff" nil
  :deps ((shell-maker gh "xenodium/shell-maker" "8c64f0b")
	       (transient gh "magit/transient" "7131bec"))
  :commands (chatgpt-shell chatgpt-shell-prompt-compose)
  :init
  (add-hook 'chatgpt-shell-mode-hook
	          (defun t/gpt-shell-hook ()
	            (setq chatgpt-shell-system-prompt "KEEP IT BRIEF, DONT MAKE STUFF UP, NO LISTS, SINGLE EXAMPLES ONLY")))
  (setq chatgpt-shell-model-version "gpt-5.2"
        chatgpt-shell-anthropic-key
	      (lambda ()
	        (t/read-first-secret :host "anthropic.com"))
	      chatgpt-shell-openai-key
	      (lambda ()
	        (t/read-first-secret :host "api.openai.com")))
  (defun t/chatgpt-shell (beg end)
    "Pop open an org mode buffer with the selection region and an optional prompt
  prepended."
    (interactive (list (and (mark t) (region-beginning))
		                   (and (mark t) (region-end))))
    (let ((reg (when (region-active-p)
		             (buffer-substring beg end)))
	        (major-mode-name (symbol-name major-mode)))
      (if (or (t/prefix-arg-universal?))
	        (chatgpt-shell-prompt-compose nil)
	      (chatgpt-shell))
      (comint-next-prompt 1)
      (run-with-timer "1sec" nil
		                  (lambda (reg)
			                  (save-excursion (when reg (insert "\n\n" reg))))
		                  reg)))
  (after! evil
    (evil-define-key 'insert chatgpt-shell-mode-map (kbd "C-d") #'t-shell-maker-ctrl-d))
  (keymap-set t-leader-map "a s" #'t/chatgpt-shell))

;;; comint
(after! comint
  (keymap-set comint-mode-map "C-a" #'comint-bol)
  (after! evil
    (evil-define-key 'insert comint-mode-map (kbd "C-a") #'comint-bol)))

;;; dired
(after! '(dired evil)
  (setq dired-use-ls-dired nil)
  (defun t-dired-k ()
    (interactive)
    (if current-prefix-arg
	      (dired-kill-subdir)
      (dired-previous-line 1)))
  (evil-set-initial-state 'dired-mode 'normal)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (evil-define-key 'normal dired-mode-map
    (kbd t-leader) t-leader-map
    ;; (kbd "<return>") #'dired-find-file
    (kbd "j") #'dired-next-line
    (kbd "k") #'t-dired-k
    (kbd "(") (cmd! (dired-hide-details-mode 1))
    (kbd ")") (cmd! (dired-hide-details-mode 0))
    (kbd "q") #'quit-window
    (kbd "<follow-link>") nil
    (kbd "<mouse-2>") nil ;; after hold
    ;; mouse always togggles folder or opens file
    (kbd "<down-mouse-1>") (cmd! (mouse-set-point last-input-event)
				                         (let ((split-window-preferred-function nil))
				                           (if (dired-subtree--dired-line-is-directory-or-link-p)
				                               (t/dired-subtree-tab)
				                             (find-file (dired-get-filename)))))
    (kbd "<tab>") #'t/dired-subtree-tab
    ;; open never split
    (kbd "<return>") (cmd! (if (t/prefix-arg-universal?)
			                         (call-interactively 'dired-find-file)
			                       (let ((split-window-preferred-function nil))
			                         (call-interactively 'dired-find-file))))))


;;; corfu
(t-package corfu gh "minad/corfu" "d2a995c" nil
  :init
  (when (require 'corfu-echo) (corfu-echo-mode))
  (when (require 'corfu-info))
  (when (require 'corfu-popupinfo) (corfu-popupinfo-mode))
  (when (require 'corfu-auto))
  (setq corfu-auto nil
	      corfu-auto-delay 0.2
	      corfu-auto-trigger "-" ;; Custom trigger characters
	      corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode)
  (defun corfu-move-to-minibuffer ()
    "Moves completion suggestions over to consult."
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
	           completion-cycle-threshold completion-cycling)
	       (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  (keymap-set corfu-map "TAB" 'corfu-move-to-minibuffer)
  (keymap-set corfu-map "<tab>" 'corfu-move-to-minibuffer)
  (keymap-set corfu-map "M-i" 'corfu-info-location)
  (keymap-set corfu-map "C-k" 'kill-line)
  (keymap-set corfu-map "C-h" 'corfu-info-documentation)
  )

;;; vertico
(t-package vertico gh "minad/vertico" "0b96e8f" nil
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (when after-init-time
    (vertico-mode))
  :hook (after-init . vertico-mode)
  :config
  (when (require 'vertico-sort nil t)
    (setq vertico-sort-function #'vertico-sort-history-alpha))
  (keymap-set vertico-map "M-?" #'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "M-TAB" #'minibuffer-complete)
  (keymap-set global-map "C-<SPC>" #'completion-at-point)
  (keymap-set global-map "C-<tab>" #'completion-at-point)
  (keymap-set global-map "C-TAB" #'completion-at-point)
  (keymap-set global-map "C-," #'embark-act))

(use-package savehist
  :init
  ;; Persist minibuffer history for Vertico and other completions.
  (savehist-mode))

(use-package emacs
  :hook (;;(after-init . pixel-scroll-mode)
	       ;; enable arrow at end of line when wrapping
	       (after-init . toggle-truncate-lines)
	       ;; indent soft wraps
	       (after-init . global-visual-wrap-prefix-mode)
	       ;; always revert
	       (after-init . global-auto-revert-mode))
  :init
  (setq-default fill-column 90 ;; prevents breaks of 89 filled paragraphs
                indent-tabs-mode nil
                tab-width 2
                standard-indent 2)
  (setq c-basic-offset 2
        js-indent-level 2
        typescript-indent-level 2
        python-indent-offset 2
        rust-indent-offset 2
        sh-basic-offset 2
        css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        yaml-indent-offset 2)
  (add-hook 'after-init-hook (cmd! (menu-bar-mode -1)))
  (add-hook 'after-init-hook (cmd! (tool-bar-mode -1)))
  :custom
  ;; vertico shows context menu in `vertico-multiform-mode'
  (context-menu-mode t)
  ;; enable indentation+completion using the tab key.
  ;; (`completion-at-point' is often bound to M-TAB.)
  ;; twice TAB opens minibuffer
  (tab-always-indent 'complete)
  ;; opening minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; do not allow the cursor in the minibuffer prompt
  ;;(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  )

;;; orderless
(t-package orderless gh "oantolin/orderless" "3a2a321" nil
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  ;; Orderless matches space-separated components in any order; basic is a fallback.
  (completion-styles '(orderless basic))
  ;; Files use partial completion (path segments) instead of orderless.
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)
  (progn
    (setq orderless-matching-styles
	        '(orderless-literal     ;; default exact substring match
	          ;; orderless-initialism
	          ;; orderless-regexp
	          ;; orderless-flex
	          ))

    (setq orderless-style-dispatchers
          '(initialism-dispatcher ;; suffix search with ,
            flex-dispatcher       ;; suffix search with .
            regexp-dispatcher     ;; suffix search with ~
            or-regexp             ;; regex search with foo|bar
            ))

    (defun regexp-dispatcher (pattern _index _total)
      "Matches regexp."
      (when (string-suffix-p "~" pattern)
	      `(orderless-regexp . ,(substring pattern 0 -1))))

    (defun flex-dispatcher (pattern _index _total)
      "Matches using any group in any order."
      (when (string-suffix-p "." pattern)
	      `(orderless-flex . ,(substring pattern 0 -1))))

    (defun or-regexp (pattern index _total)
      "foo|bar"
      (cond
       ((string-suffix-p "|" pattern)
	      `(orderless-regexp . ,(concat "\\(" (concat (s-replace "|" "\\|" (substring pattern 0 -1)) "\\)"))))
       ((string-match-p "|" pattern)
	      `(orderless-regexp . ,(concat "\\(" (concat (s-replace "|" "\\|" pattern) "\\)"))))))

    (defun literal-dispatcher (pattern _index _total)
      "Literal style dispatcher using the equals sign as a suffix."
      (when (string-suffix-p "=" pattern)
	      `(orderless-literal . ,(substring pattern 0 -1))))

    ;;;###autoload
    (defun initialism-dispatcher (pattern _index _total)
      "Matches leading on words in order
E.g.
#fun#gjp, ha,
(defun t/js2-get-json-path (&optional hardcoded-array-index))
 ^^^^^       ^   ^    ^               ^         ^
#fun#gjp, hi,
Would not match the above as no leading words start h then another word starting with i
"
      (when (string-suffix-p "," pattern)
	      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

    (defun orderless-strict-initialism (component)
      "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
      (orderless--separated-by
          '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
	      (cl-loop for char across component collect `(seq word-start ,char)))))
  )

;;; consult
(t-package consult gh "minad/consult" "f8c2ef5" nil
  :commands (consult-buffer
	           consult-line
	           consult-line-multi
	           consult-ripgrep
	           consult-git-grep
	           consult-grep
	           consult-find
	           consult-man
	           consult-completion-in-region))

;;; marginalia
(t-package marginalia gh "minad/marginalia" "d28a5e5" nil
  :config
  (marginalia-mode 1))

;;; embark
(t-package embark gh "oantolin/embark" "e023888" nil
  :commands (embark-act)
  :deps ((compat gh "emacs-compat/compat" "38df650")))

;;; wgrep
(t-package wgrep gh "mhayashi1120/Emacs-wgrep" "49f09ab" nil)

;;; pulsar
(t-package pulsar gh "protesilaos/pulsar" "70956bf" nil
  :commands (pulsar-pulse-line pulsar-pulse-region)
  :init
  (dolist (fn '(comint-next-prompt
		            comint-previous-prompt))
    (advice-add (quote fn) :after (cmd! (pulsar-pulse-line))))
  (after! evil
    (dolist (fn '(evil-window-right
		              evil-window-bottom
		              evil-window-left
		              evil-window-up))
      (advice-add (quote fn) :after (cmd! (pulsar-pulse-line)))))
  :config
  (setq pulsar-pulse-on-window-change t)
  (pulsar-global-mode 1))

;;; air
(t-package spacious-padding gh "protesilaos/spacious-padding" "a9cddfb" nil
  :commands (spacious-padding-mode)
  :hook (after-init . spacious-padding-mode))

;;; git gutter
(t-package diff-hl gh "dgutov/diff-hl" "bb9af85" nil
  :deps ((cl-lib sav "emacs/elpa.git" "790948a"))
  :init
  (setq diff-hl-show-staged-changes nil)
  :config
  (global-diff-hl-mode))

;;; lang:nix
(t-package nix-ts-mode gh "nix-community/nix-ts-mode" "3198317" nil
  :mode (("\\.nix\\'" . nix-ts-mode)))

;;; lang: markdown
(t-package markdown-mode gh "jrblevin/markdown-mode" "107a368" nil
  :commands markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config
  (markdown-toggle-url-hiding))

;;; lang: elisp
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(t/set-pairs 'emacs-lisp-mode '((?` . ?')))

;;; lang: prog
(add-hook 'prog-mode-hook 'outline-minor-mode)

;;; emojis :rocket:
(t-package emojify gh "iqbalansari/emacs-emojify" "1b72641" nil
  :commands (global-emojify-mode)
  :deps ((dash gh "magnars/dash.el" "d3a84021")
         (ht gh "Wilfred/ht.el" "1c49aad")))

;;; apps
(keymap-set t-leader-map "o a a" #'org-agenda)
(keymap-set t-leader-map "o w" #'eww)
(keymap-set t-leader-map "o o" (cmd! (let ((default-directory (t/org-file)))
				                               (call-interactively 'find-file))))
(keymap-set t-leader-map "o h" #'hnreader-news)
(keymap-set t-leader-map "r r" 't/read)

;;; snippets
(use-package tempo
  :config
  (tempo-define-template
   "js-log"
   '("console.log(" (p) ");" >)
   "js-log"
   "Insert a defun skeleton.")
  (tempo-define-template
   "elisp-defun"
   '("(defun" (p "Name: ") " (" (p "Args: ") ")\n  \"\" \n  (interactive)\n  " p "\n)"
     >)
   "elisp-defun"
   "Insert a defun skeleton.")
  )

(define-abbrev-table 'global-abbrev-table
  '(("fun" "" tempo-template-elisp-defun 0)
    ("log" "" tempo-template-js-log 0)))

;;; hackernews
(t-package hnreader gh "thanhvg/emacs-hnreader" "a56f67a" nil
  :deps ((request gh "tkf/emacs-request" "6f419b5")
	       (promise gh "chuntaro/emacs-promise" "cec51fe"))
  :commands (hnreader-news))

;;; browser
(use-package eww
  :config
  (after! evil
    (evil-define-key 'normal eww-mode-map (kbd t-leader) t-leader-map))
  (keymap-set eww-mode-map "<triple-wheel-left>" 'eww-back-url)
  (keymap-set eww-mode-map "<triple-wheel-right>" 'eww-forward-url)
  (keymap-set eww-mode-map "<mouse-8>" 'eww-back-url)
  (keymap-set eww-mode-map "<mouse-9>" 'eww-forward-url)
  (add-hook 'eww-after-render-hook (cmd! (call-interactively 'eww-readable)))
  (add-hook 'eww-after-render-hook 'olivetti-mode))

;;; eglot
(use-package eglot)

(after! eglot
  (setq eglot-connect-timeout (* 60 20)
        ;; don't block while waiting, defaults to 3
        eglot-sync-connect nil))

;;; apheleia
(t-package apheleia gh "radian-software/apheleia" "e6e5d55" nil
  :config
  (apheleia-global-mode +1))

;;; org
(use-package org
  :init
  (defun t/org-file (&optional file)
    (concat (expand-file-name "~/Dropbox/org") "/" file))
  (setq org-directory (t/org-file)
	      org-agenda-files (list org-directory)
	      org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'"
	      org-archive-location "%s_archive.gpg::" ; so files are encrypted automatically
	      org-attach-directory (t/org-file "attachments/")
	      org-id-method 'ts  ; 'uuid is default
	      org-special-ctrl-k t     ; don't clear tags, etc
	      org-special-ctrl-a/e t   ; don't move past ellipsis on c-e
	      org-log-done 'time       ; log when todos are completed
	      org-log-redeadline 'time ; log when deadline changes
	      org-log-reschedule 'time ; log when schedule changes
	      org-reverse-note-order t ; newest notes first
        org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)"))
	      org-default-notes-file (t/org-file "tasks.org")
        org-capture-templates
        `(("t" "Task" entry (file+olp org-default-notes-file "tasks") "* TODO %? \n\n%i\n\n" :prepend t :empty-lines-after 1)
          ("d" "Da" entry (file+olp ,(t/org-file "da.org.gpg") "Tasks") "* TODO %? \n\n%i" :prepend t :empty-lines-after 1)
          ("b" "Bekk" entry (file+olp ,(t/org-file "bekk.org.gpg") "Tasks") "* TODO %? \n\n%i" :prepend t :empty-lines-after 1)
          ("f" "File/item (or elfeed)" entry (file+olp org-default-notes-file "Tasks") "* TODO %? %^G\n\n%i%a\n\n" :prepend t :empty-lines-after 1)
          ("l" "Link (eww, mu4e, etc)" entry (file+olp org-default-notes-file "Tasks") (function t/org-capture-link-template) :prepend t :empty-lines-after 1)
          ("c" "Chrome location" entry (file+olp org-default-notes-file "Tasks") (function t/org-capture-chrome-link-template) :prepend t :empty-lines-after 1)
          ("p" "Post" entry (file+olp "~/Code/posts/content-org/blog.org" "Drafts") (function org-hugo-new-subtree-post-capture-template))))
  (defun t/org-double-mouse-1 (event)
    "Toggle heading visibility when double-clicking an Org heading."
    (interactive "e")
    (mouse-set-point event)
    (when (org-at-heading-p)
      (org-cycle)))
  :config
  (keymap-set org-mode-map "<double-mouse-1>" #'t/org-double-mouse-1)
  (t/set-pairs 'org-mode '((?~ . ?~) (?= . ?=) (?` . ?') (?< . ?>) (?/ . ?/) (?« . ?») (?_ . ?_)))
  (after! evil 
    (evil-set-initial-state 'org-agenda-mode 'motion))
  (after! (evil org-agenda)
    (evil-define-key 'motion org-agenda-keymap (kbd "H") #'org-agenda-earlier)
    (evil-define-key 'motion org-agenda-keymap (kbd "L") #'org-agenda-later)
    (evil-define-key 'motion org-agenda-keymap (kbd "l") #'org-agenda-log-mode)
    (evil-define-key 'motion org-agenda-keymap (kbd "d") #'org-agenda-day-view)
    (evil-define-key 'motion org-agenda-keymap (kbd "w") #'org-agenda-week-view)
    (evil-define-key 'motion org-agenda-keymap (kbd "y") #'org-agenda-year-view)
    (evil-define-key 'motion org-agenda-keymap (kbd "m") #'org-agenda-month-view)))

;;; pomodoro
(t-package org-pomodoro gh "marcinkoziej/org-pomodoro" "3f5bcfb8" nil
  :commands (org-pomodoro)
  :deps ((alert gh "jwiegley/alert" "79f6936a")
         (cl-lib sav "emacs/elpa.git" "790948a")))

;;; alerts
(t-package org-alert gh "spegoraro/org-alert" "0bc04cea" nil
  :commands (org-alert-enable org-alert-disable org-alert-deadlines)
  :deps ((alert gh "jwiegley/alert" "79f6936a")
	       (cl-lib sav "emacs/elpa.git" "790948a"))
  :init
  (setq alert-default-style (if is-mac 'osx-notifier 'libnotify))
  (setq org-alert-interval (* 5 60)
	      org-alert-notify-cutoff 5
	      org-alert-notify-after-event-cutoff 5
	      org-alert-time-match-string "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\).*>")
  :config
  (org-alert-enable))

;;; mail
(t-package notmuch gh "notmuch/notmuch" "076d597" "emacs"
  :commands (notmuch)
  :init
  (after! notmuch
    (require 'notmuch-address)
    ;; address completion in common recipient headers
    (dolist (h '("To" "Cc" "Bcc"))
      (add-to-list 'message-completion-alist (cons h #'notmuch-address-expand-name)))
    ;; file outgoing mail
    (setq notmuch-fcc-dirs "sent +sent -inbox"))
  (setq notmuch-identities '(("Torgeir Thoresen <torgeir.thoresen@gmail.com>")))
  (setq notmuch-show-logo nil
	      notmuch-hello-auto-refresh t
	      notmuch-search-oldest-first nil
	      notmuch-show-empty-saved-searches t
	      notmuch-saved-searches
        `((:name "📥 inbox"
		             :query "tag:inbox"
		             :sort-order newest-first
		             :key ,(kbd "i"))
          (:name " inbox today"
		             :query "date:today"
		             :sort-order newest-first
		             :key ,(kbd "t"))
          (:name "✉️ all unread (inbox)"
		             :query "tag:unread and tag:inbox"
		             :sort-order newest-first
		             :key ,(kbd "u"))))
  (setq notmuch-archive-tags nil ; dont archive email
	      notmuch-show-text/html-blocked-images "." ; block everything
        notmuch-message-replied-tags '("+replied")
        notmuch-message-forwarded-tags '("+forwarded")
        notmuch-show-mark-read-tags '("-unread")
        notmuch-draft-tags '("+draft")
        notmuch-draft-folder "drafts"
        notmuch-draft-save-plaintext 'ask)
  (add-hook 'notmuch-show-mode-hook 'olivetti-mode)
  (add-hook 'notmuch-show-mode-hook 't/read)
  (add-hook 'notmuch-search-mode-hook 'olivetti-mode)
  (keymap-set t-leader-map "o m" 'notmuch))

(setq user-full-name "Torgeir Thoresen"
      user-mail-address "torgeir.thoresen@gmail.com")

(setq sendmail-program (executable-find "msmtp")
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-send-mail-function #'message-send-mail-with-sendmail)

(defun t/fetch-mail ()
  (interactive)
  (epa-file-enable)
  (auth-source-forget-all-cached)
  (message "Fetching..")
  (async-shell-command "notmuch new" "*t-notmuch-out*" "*t-notmuch-err*"))

(add-to-list
 'display-buffer-alist '("^\\*t-notmuch-out\\*$"
                         (display-buffer-no-window)
                         (allow-no-window . t)))

;;; epa encrypt org
(after! auth-source (setq auth-sources '("~/.authinfo.gpg")))
(after! epa
  (setq-default epa-file-encrypt-to '("torgeir@keybase.io"))
  ;; https://irreal.org/blog/?p=11827
  (fset 'epg-wait-for-status 'ignore))
;; (after! epa-file (epa-file-enable))
(add-hook 'server-after-make-frame-hook 'epa-file-enable)
(add-hook 'org-mode-hook 'epa-file-enable) 
(add-hook 'server-after-make-frame-hook 'auth-source-forget-all-cached)

;;; rainbows
(defun t/rainbow ()
  (interactive)
  ;; Show paren match face
  (set-face-attribute 'show-paren-match nil :background 'unspecified :foreground "SpringGreen" :weight 'bold)
  ;; Rainbow delimiters faces
  (let ((colors '("DeepPink4" "DeepPink3" "DeepPink2" "DeepPink1"
		              "maroon4" "maroon3" "maroon2" "maroon1"
		              "VioletRed3"))
	      (face-fmt "rainbow-delimiters-depth-%d-face"))
    (dotimes (i (length colors))
      (set-face-attribute (intern (format face-fmt (1+ i))) nil :foreground (nth i colors) :overline nil :underline nil)))
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "Red" :bold t :overline nil :underline nil)
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil :foreground "Orange" :bold t :overline nil :underline nil))

(t-package rainbow-delimiters gh "Fanael/rainbow-delimiters" "f40ece5" nil
  :hook (;;(after-change-major-mode . rainbow-delimiters-mode)
	       (prog-mode . rainbow-delimiters-mode))
  :init
  (require 'rainbow-delimiters)
  :config
  (t/rainbow))

;;; transparency
(defun t/transparency (&optional v)
  "Sets the transparency of the frame window. 0=transparent/100=opaque."
  (interactive)
  (let* ((f (window-frame (selected-window)))
         (v (or v
                (string-to-number
                 (read-string (format "Opacity is %s. Enter a value from 0 - 100, or press enter for 100: "
                                      (frame-parameter f 'alpha))
                              nil nil "100")))))
    (set-frame-parameter f 'alpha v)
    (message "Remember to run m-x redraw-display.")))

(defun t/org-clock-start (&optional &rest args)
  (interactive)
  (when (not (featurep 'org-pomodoro))
    (require 'org-pomodoro))
  (org-todo "STARTED"))
(defun t/org-clock-stop (&optional &rest args)
  (interactive)
  (when (not (featurep 'org-pomodoro))
    (require 'org-pomodoro))
  (when (not (org-pomodoro-active-p))
    (org-clock-jump-to-current-clock)
    (org-todo)))
(advice-remove 'org-clock-in 't/org-clock-start)
(advice-remove 'org-clock-out 't/org-clock-stop)
(advice-add 'org-clock-in :after 't/org-clock-start)
(advice-add 'org-clock-out :after 't/org-clock-stop)

;;; TODO should be on modeline
(defun t/tasks-left ()
  (interactive)
  (if (not (get-buffer "tasks.org"))
      "n/a"
    (with-current-buffer "tasks.org"
      (let ((count 0))
        ;; for each heading
        (org-map-entries
         (lambda (&optional heading)
           (when (not (org-entry-is-done-p))
             (setq count (1+ count))))
         ;; all headline
         t
         ;; in file
         'file)
        ;; needs to be string
        (format "%s" count)))))


(after! evil
  (evil-set-register ?c [?: ?s ?/ ?\\ ?\( ?\[ ?a ?- ?z ?0 ?- ?9 ?\] ?\\ ?\) ?\\ ?\( ?\[ ?A ?- ?Z ?0 ?- ?9 ?\] ?\\ ?\) ?/ ?\\ ?1 ?_ ?\\ ?l ?\\ ?2 ?/ ?g])
  (evil-set-register ?s [?: ?s ?/ ?\\ ?\( ?\[ ?a ?- ?z ?0 ?- ?9 ?\] ?\\ ?\) ?_ ?\\ ?\( ?\[ ?a ?- ?z ?0 ?- ?9 ?\] ?\\ ?\) ?/ ?\\ ?1 ?\\ ?u ?\\ ?2 ?/ ?g]))

;;; -- stuff not brought over --

;;; org mode + heading functions + eldoc ping

;;; deno

;;; evil-cleverparens

;;; tree-sitter

;;; goggles
;; https://github.com/edkolev/evil-goggles

;;; goto next error/flymake

;;; nerd icons dired

;;; own modules

;;; winner undo redo

;;; highlight symbol

;;; commit semantic

;;; copilot
;; TODO needs some npm package
;; (t-package copilot gh "copilot-emacs/copilot.el" "59a4a29" nil
;;   :deps ((editorconfig gh "editorconfig/editorconfig-emacs" "b18fcf7")
;;          (track-changes gh "emacs-straight/track-changes" "6d8fb08")
;;          (compat gh "emacs-compat/compat" "38df650"))
;;   :config
;;   (keymap-set t-leader-map "t c" 'copilot-mode))

;;; direnv
(t-package direnv gh "wbolster/emacs-direnv" "c0bf3b8" nil)

;;; yaml
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(comment t-package exec-path-from-shell gh "purcell/exec-path-from-shell" "7552abf" nil
         :commands (exec-path-from-shell-initialize))

;;; timers are useful
(put 'list-timers 'disabled nil)

;;; git timetravel
(t-package git-timemachine cb "pidu/git-timemachine" "d1346a7" nil
  :init
  (keymap-set t-leader-map "g T" 'git-timemachine))
