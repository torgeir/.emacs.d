;; --- Typography stack -------------------------------------------------------
(set-face-attribute 'default nil
                    :height 140 :weight 'light :family "Roboto Mono")
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; --- Frame / windows layout & behavior --------------------------------------
(setq default-frame-alist
      '(;;(height . 44) (width  . 81)
        (left-fringe . 0) (right-fringe . 0)
        (internal-border-width . 32) (vertical-scroll-bars . nil)
        (bottom-divider-width . 0) (right-divider-width . 0)
        (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)
;;(setq-default pop-up-windows nil)

;; --- Minimal NANO (not a real) theme ----------------------------------------
(defface nano-default '((t)) "")   (defface nano-default-i '((t)) "")
(defface nano-highlight '((t)) "") (defface nano-highlight-i '((t)) "")
(defface nano-subtle '((t)) "")    (defface nano-subtle-i '((t)) "")
(defface nano-faded '((t)) "")     (defface nano-faded-i '((t)) "")
(defface nano-salient '((t)) "")   (defface nano-salient-i '((t)) "")
(defface nano-popout '((t)) "")    (defface nano-popout-i '((t)) "")
(defface nano-strong '((t)) "")    (defface nano-strong-i '((t)) "")
(defface nano-critical '((t)) "")  (defface nano-critical-i '((t)) "")

(defun nano-set-face (name &optional foreground background weight)
  "Set NAME and NAME-i faces with given FOREGROUND, BACKGROUND and WEIGHT"

  (apply #'set-face-attribute `(,name nil
                                      ,@(when foreground `(:foreground ,foreground))
                                      ,@(when background `(:background ,background))
                                      ,@(when weight `(:weight ,weight))))
  (apply #'set-face-attribute `(,(intern (concat (symbol-name name) "-i")) nil
                                :foreground ,(face-background 'nano-default)
                                ,@(when foreground `(:background ,foreground))
                                :weight regular)))

(defun nano-link-face (sources faces &optional attributes)
  "Make FACES to inherit from SOURCES faces and unspecify ATTRIBUTES."

  (let ((attributes (or attributes
                        '( :foreground :background :family :weight
                           :height :slant :overline :underline :box))))
    (dolist (face (seq-filter #'facep faces))
      (dolist (attribute attributes)
        (set-face-attribute face nil attribute 'unspecified))
      (set-face-attribute face nil :inherit sources))))

(defun nano-install-theme ()
  "Install THEME"

  (set-face-attribute 'default nil
                      :foreground (face-foreground 'nano-default)
                      :background (face-background 'nano-default))
  (dolist (item '((nano-default .  (variable-pitch variable-pitch-text
                                                   fixed-pitch fixed-pitch-serif))
                  (nano-highlight . (hl-line highlight))
                  (nano-subtle .    (match region
                                           lazy-highlight widget-field))
                  (nano-faded .     (shadow
                                     font-lock-comment-face
                                     font-lock-doc-face
                                     icomplete-section
                                     completions-annotations))
                  (nano-popout .    (warning
                                     font-lock-string-face))
                  (nano-salient .   (success link
                                             help-argument-name
                                             custom-visibility
                                             font-lock-type-face
                                             font-lock-keyword-face
                                             font-lock-builtin-face
                                             completions-common-part
                                             dired-directory))
                  (nano-strong .    (font-lock-function-name-face
                                     font-lock-variable-name-face
                                     icomplete-first-match
                                     minibuffer-prompt))
                  (nano-critical .  (error
                                     completions-first-difference))
                  (nano-faded-i .   (help-key-binding))
                  (nano-default-i . (custom-button-mouse
                                     isearch))
                  (nano-critical-i . (isearch-fail))
                  ((nano-subtle nano-strong) . (custom-button
                                                icomplete-selected-match))
                  ((nano-faded-i nano-strong) . (show-paren-match))))
    (nano-link-face (car item) (cdr item)))

  ;; Mode & header lines
  (set-face-attribute 'header-line nil
                      :background 'unspecified
                      :underline nil
                      :box `( :line-width 1
                              :color ,(face-background 'nano-default))
                      :inherit 'nano-subtle)
  (let ((bg (face-background 'default)))
    (dolist (face '(mode-line mode-line-active mode-line-inactive))
      (set-face-attribute face nil
                          :background bg :foreground (face-foreground 'nano-faded)
                          :underline nil :overline nil
                          :height 1.0 :box `(:line-width 1 :color ,bg))))
  )

(defun nano-flatwhite-highlights (kw-fg kw-bg str-fg str-bg const-fg const-bg var-fg var-bg)
  "Apply flatwhite-style tinted background highlights to key font-lock faces.
Each pair (FG BG) tints keywords, strings, constants, and variables
with a pastel background + darker same-hue foreground."
  (dolist (spec `((font-lock-keyword-face       ,kw-fg    ,kw-bg)
                  (font-lock-builtin-face        ,kw-fg    ,kw-bg)
                  (font-lock-string-face         ,str-fg   ,str-bg)
                  (font-lock-string-delimiter-face ,str-fg ,str-bg)
                  (font-lock-constant-face       ,const-fg ,const-bg)
                  (font-lock-variable-name-face  ,var-fg   ,var-bg)))
    (set-face-attribute (car spec) nil
                        :foreground (cadr spec)
                        :background (caddr spec))))

(defun nano-light (&rest args)
  "NANO light theme (based on material colors) with flatwhite-style bg highlights"

  (interactive)
  (nano-set-face 'nano-default "#37474F" "#FFFFFF") ;; Blue Grey / L800
  (nano-set-face 'nano-strong "#000000" nil 'regular) ;; Black
  (nano-set-face 'nano-highlight nil "#FAFAFA") ;; Very Light Grey
  (nano-set-face 'nano-subtle nil "#ECEFF1") ;; Blue Grey / L50
  (nano-set-face 'nano-faded "#90A4AE") ;; Blue Grey / L300
  (nano-set-face 'nano-salient "#673AB7") ;; Deep Purple / L500
  (nano-set-face 'nano-popout "#FFAB91") ;; Deep Orange / L200
  (nano-set-face 'nano-critical "#FF6F00") ;; Amber / L900
  (nano-install-theme)
  (nano-flatwhite-highlights
   "#4527A0" "#EDE7F6"   ;; keywords: deep purple text / L50 bg
   "#2E7D32" "#F1F8E9"   ;; strings:  dark green text / L50 bg
   "#00695C" "#E0F2F1"   ;; constants: dark teal text / L50 bg
   "#1A237E" "#E8EAF6")) ;; variables: indigo text / L50 bg

(defun nano-dark (&rest args)
  "NANO dark theme (based on nord colors) with flatwhite-style bg highlights"

  (interactive)
  (nano-set-face 'nano-default "#ECEFF4" "#2E3440") ;; Snow Storm 3
  (nano-set-face 'nano-strong "#ECEFF4" nil 'regular) ;; Polar Night 0
  (nano-set-face 'nano-highlight nil "#3B4252")  ;; Polar Night 1
  (nano-set-face 'nano-subtle nil "#434C5E") ;; Polar Night 2
  (nano-set-face 'nano-faded "#677691") ;;
  (nano-set-face 'nano-salient "#81A1C1")  ;; Frost 2
  (nano-set-face 'nano-popout "#D08770") ;; Aurora 1
  (nano-set-face 'nano-critical (plist-get t-colors :head))
  (nano-install-theme)
  (nano-flatwhite-highlights
   "#C397D8" "#352A4A"   ;; keywords: lavender / dark purple bg
   "#A3BE8C" "#2B3B2B"   ;; strings:  aurora green / dark green bg
   "#8FBCBB" "#1E3535"   ;; constants: frost teal / dark teal bg
   "#88C0D0" "#253545")) ;; variables: frost blue / dark blue bg

;; --- Command line theme chooser ---------------------------------------------
(add-to-list 'command-switch-alist '("-dark"  . nano-dark))
(add-to-list 'command-switch-alist '("-light" . nano-light))
(if (member "-light" command-line-args) (nano-light) (nano-dark))

;; --- Header & mode lines ----------------------------------------------------
(setq-default mode-line-format
              '(:eval (when (mode-line-window-selected-p) (persp-mode-line))))
(setq-default header-line-format
              '(:eval
                (let* ((prefix (cond (buffer-read-only     '("RO" . nano-default-i))
                                     ((buffer-modified-p)  '("**" . nano-critical-i))
                                     (t                    '("RW" . nano-faded-i))))
                       (minimal (string-prefix-p ":" (buffer-name)))
                       (mode (unless minimal
                               (concat "(" (downcase (cond ((consp mode-name) (car mode-name))
                                                           ((stringp mode-name) mode-name)
                                                           (t "unknown")))
                                       " mode)")))
                       (coords (format-mode-line "%c:%l "))
                       (p-len 0))
                  (list
                   (propertize " " 'face (cdr prefix)  'display '(raise -0.25))
                   (propertize (car prefix) 'face (cdr prefix))
                   (propertize " " 'face (cdr prefix) 'display '(raise +0.25))
                   (propertize (format-mode-line " %b ") 'face 'nano-strong)
                   (when mode (propertize mode 'face `(:foreground ,(plist-get t-colors :head))))
                   (propertize " " 'display `(space :align-to (- right ,(+ (length coords) p-len 2))))
                   (propertize coords 'face 'nano-faded)
                   ))))

;; --- Minibuffer setup -------------------------------------------------------
(defun nano-minibuffer--setup ()
  (set-window-margins nil 3 0)
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (+ (point-min) 1)
                         `(display ((margin left-margin)
                                    ,(format "# %s" (substring (minibuffer-prompt) 0 1))))))
  (setq truncate-lines t))
(add-hook 'minibuffer-setup-hook #'nano-minibuffer--setup)
