;; -*- lexical-binding: t -*-
;; --- Typography stack -------------------------------------------------------
(set-face-attribute 'default nil
                    ;; :height 140
                    :weight 'light
                    ;; :family "Roboto Mono"
                    )
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)
(defface nano-truncation '((t)) "")
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?… 'nano-truncation))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?↵ 'nano-truncation))

;; --- Frame / windows layout & behavior --------------------------------------
(setq default-frame-alist
      '(;;(height . 44) (width  . 81)
        ;; (left-fringe . 0) (right-fringe . 0)
        (internal-border-width . 32) (vertical-scroll-bars . nil)
        (bottom-divider-width . 32) (right-divider-width . 32)
        (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)
;;(setq-default pop-up-windows nil)

;; --- Minimal NANO (not a real) theme ----------------------------------------
(defface nano-default   '((t)) "") (defface nano-default-i   '((t)) "")
(defface nano-highlight '((t)) "") (defface nano-highlight-i '((t)) "")
(defface nano-subtle    '((t)) "") (defface nano-subtle-i    '((t)) "")
(defface nano-faded     '((t)) "") (defface nano-faded-i     '((t)) "")
(defface nano-salient   '((t)) "") (defface nano-salient-i   '((t)) "")
(defface nano-popout    '((t)) "") (defface nano-popout-i    '((t)) "")
(defface nano-strong    '((t)) "") (defface nano-strong-i    '((t)) "")
(defface nano-critical  '((t)) "") (defface nano-critical-i  '((t)) "")

(defun nano-set-face (name &optional foreground background weight)
  "Set NAME and NAME-i faces with given FOREGROUND, BACKGROUND and WEIGHT"

  (apply #'set-face-attribute `( ,name nil
                                 ,@(when foreground `(:foreground ,foreground))
                                 ,@(when background `(:background ,background))
                                 ,@(when weight `(:weight ,weight))))
  (apply #'set-face-attribute `( ,(intern (concat (symbol-name name) "-i")) nil
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

  (set-face-attribute 'header-line nil
                      :background 'unspecified
                      :underline nil
                      :box nil
                      :inherit 'nano-subtle)
  (let ((bg (face-background 'default)))
    (dolist (face '(mode-line mode-line-active mode-line-inactive))
      (set-face-attribute face nil
                          :background bg :foreground (face-foreground 'nano-faded)
                          :underline nil :overline nil
                          :height 1
                          :box `(:line-width 5 :color ,bg)))
    (set-face-attribute 'fringe nil :background bg)
    (set-face-attribute 'fringe nil :background bg :height 1.0 :box `(:line-width 1 :color ,bg)))
  (set-face-attribute 'nano-truncation nil
                      :foreground (face-background 'nano-subtle)
                      :background 'unspecified)
  (set-fringe-bitmap-face 'left-curly-arrow  'nano-truncation)
  (set-fringe-bitmap-face 'right-curly-arrow 'nano-truncation)
  (set-fringe-bitmap-face 'right-arrow       'nano-truncation)
  (set-fringe-bitmap-face 'left-arrow        'nano-truncation))

(defun nano-set-dired-subtree-faces ()
  "Set dired-subtree depth faces to progressively darkened theme background."
  (require 'color)
  (let ((bg (face-background 'default)))
    (cl-loop for depth from 1 to 10
             do (let ((face (intern (format "dired-subtree-depth-%d-face" depth))))
                  (unless (facep face) (make-face face))
                  (set-face-attribute face nil
                                      :background (color-darken-name bg (* depth 2)))))))

(defun nano-set-magit-faces (specs)
  "Set magit faces from SPECS, a list of (face fg bg). nil leaves that attribute unspecified."
  (dolist (spec specs)
    (when (facep (car spec))
      (set-face-attribute (car spec) nil
                          :foreground (or (cadr spec)  'unspecified)
                          :background (or (caddr spec) 'unspecified)))))

(defun nano-set-ansi-colors (colors)
  "Set ansi-color faces from COLORS, a 16-element list: normal then bright."
  (cl-loop for name in '(black red green yellow blue magenta cyan white
                               bright-black bright-red bright-green bright-yellow
                               bright-blue bright-magenta bright-cyan bright-white)
           for color in colors
           do (set-face-attribute (intern (format "ansi-color-%s" name))
                                  nil :foreground color :background color)))

(defun nano-flatwhite-highlights (kw-fg kw-bg str-fg str-bg const-fg const-bg var-fg var-bg)
  "Apply flatwhite-style tinted background highlights to key font-lock faces.
Each pair (FG BG) tints keywords, strings, constants, and variables
with a pastel background + darker same-hue foreground."
  (dolist (spec `((font-lock-keyword-face        ,kw-fg    ,kw-bg)
                  (font-lock-builtin-face        ,kw-fg    ,kw-bg)
                  (font-lock-string-face         ,str-fg   ,str-bg)
                  ;; (font-lock-string-delimiter-face ,str-fg ,str-bg)
                  (font-lock-constant-face       ,const-fg ,const-bg)
                  (font-lock-variable-name-face  ,var-fg   ,var-bg)))
    (set-face-attribute (car spec) nil
                        :foreground (cadr spec)
                        :background (caddr spec))))

(defun nano-light (&rest args)
  "NANO light theme (based on material colors) with flatwhite-style bg highlights"
  (interactive)
  (nano-set-face 'nano-default "#37474F" "#FAFAFA") ;; Blue Grey / L800 / light grey
  (nano-set-face 'nano-strong "#000000" nil 'regular) ;; Black
  (nano-set-face 'nano-highlight nil "#F0F0F0") ;; Light Grey
  (nano-set-face 'nano-subtle nil "#ECEFF1") ;; Blue Grey / L50
  (nano-set-face 'nano-faded "#90A4AE") ;; Blue Grey / L300
  (nano-set-face 'nano-salient "#81A1C1") ;; Frost 2
  (nano-set-face 'nano-popout "#FFAB91") ;; Deep Orange / L200
  (nano-set-face 'nano-critical "#FF6F00") ;; Amber / L900
  (nano-install-theme)
  (nano-flatwhite-highlights
   "#4527A0" "#EDE7F6"   ;; keywords: deep purple text / L50 bg
   "#2E7D32" "#F1F8E9"   ;; strings:  dark green text / L50 bg
   "#00695C" "#E0F2F1"   ;; constants: dark teal text / L50 bg
   "#1A237E" "#E8EAF6")  ;; variables: indigo text / L50 bg
  (nano-set-ansi-colors
   '(;;  normal colors
     "#546E7A"  ;; 0  black   — Blue Grey L600
     "#C62828"  ;; 1  red     — Red L800
     "#2E7D32"  ;; 2  green   — Dark Green (= strings)
     "#FF8F00"  ;; 3  yellow  — Amber L800
     "#1565C0"  ;; 4  blue    — Blue L800
     "#4527A0"  ;; 5  magenta — Deep Purple (= keywords)
     "#00695C"  ;; 6  cyan    — Dark Teal (= constants)
     "#B0BEC5"  ;; 7  white   — Blue Grey L200
     ;; bright colors
     "#90A4AE"  ;; 8  bright-black   — Blue Grey L300 (faded)
     "#E53935"  ;; 9  bright-red     — Red L600
     "#43A047"  ;; 10 bright-green   — Green L600
     "#FFB300"  ;; 11 bright-yellow  — Amber L600
     "#1E88E5"  ;; 12 bright-blue    — Blue L600
     "#673AB7"  ;; 13 bright-magenta — Deep Purple L500 (salient)
     "#00897B"  ;; 14 bright-cyan    — Teal L600
     "#ECEFF1")) ;; 15 bright-white  — Blue Grey L50
  (nano-set-magit-faces
   '(;; section
     (magit-section-heading           "#673AB7" nil)       ;; salient
     (magit-section-highlight         nil       "#ECEFF1") ;; subtle
     (magit-section-heading-selection "#FF6F00" nil)       ;; critical
     ;; branches / refs
     (magit-branch-local              "#1565C0" nil)
     (magit-branch-remote             "#2E7D32" nil)       ;; = strings
     (magit-branch-remote-head        "#2E7D32" nil)
     (magit-branch-current            "#4527A0" nil)       ;; = keywords
     (magit-tag                       "#FF8F00" nil)       ;; amber
     (magit-hash                      "#90A4AE" nil)       ;; faded
     (magit-dimmed                    "#90A4AE" nil)
     ;; log
     (magit-log-author                "#C62828" nil)
     (magit-log-date                  "#90A4AE" nil)
     (magit-log-graph                 "#B0BEC5" nil)
     ;; process
     (magit-process-ok                "#2E7D32" nil)
     (magit-process-ng                "#C62828" nil)
     ;; diffstat
     (magit-diffstat-added            "#2E7D32" nil)
     (magit-diffstat-removed          "#C62828" nil)
     ;; diff added/removed
     (magit-diff-added                "#2E7D32" "#F1F8E9") ;; = string hues
     (magit-diff-added-highlight      "#1B5E20" "#C8E6C9")
     (magit-diff-removed              "#B71C1C" "#FFEBEE")
     (magit-diff-removed-highlight    "#B71C1C" "#FFCDD2")
     ;; diff context
     (magit-diff-context              "#90A4AE" nil)
     (magit-diff-context-highlight    "#546E7A" "#F5F5F5")
     ;; diff hunk headings
     (magit-diff-hunk-heading         "#546E7A" "#ECEFF1")
     (magit-diff-hunk-heading-highlight "#37474F" "#CFD8DC")
     (magit-diff-hunk-heading-selection "#FF6F00" "#CFD8DC")
     ;; diff file headings
     (magit-diff-file-heading         "#37474F" nil)
     (magit-diff-file-heading-highlight "#37474F" "#E8EAF6")
     (magit-diff-file-heading-selection "#FF6F00" "#E8EAF6")
     ;; diff revision summary
     (magit-diff-revision-summary          "#37474F" "#ECEFF1")
     (magit-diff-revision-summary-highlight "#37474F" "#CFD8DC")
     ;; blame
     (magit-blame-heading   "#37474F" "#ECEFF1")
     (magit-blame-margin    "#37474F" "#ECEFF1")
     (magit-blame-highlight "#37474F" "#CFD8DC")
     (magit-blame-dimmed    "#90A4AE" nil)))
  (nano-set-dired-subtree-faces))

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
   "#88C0D0" "#253545")  ;; variables: frost blue / dark blue bg
  (nano-set-ansi-colors
   '(;;  normal colors
     "#3B4252"  ;; 0  black   — Polar Night 1
     "#BF616A"  ;; 1  red     — Aurora 0
     "#A3BE8C"  ;; 2  green   — Aurora 4 (= strings)
     "#EBCB8B"  ;; 3  yellow  — Aurora 3
     "#81A1C1"  ;; 4  blue    — Frost 2 (salient)
     "#B48EAD"  ;; 5  magenta — Aurora 5
     "#8FBCBB"  ;; 6  cyan    — Frost 0 (= constants)
     "#D8DEE9"  ;; 7  white   — Snow Storm 1
     ;; bright colors
     "#4C566A"  ;; 8  bright-black   — Polar Night 3
     "#BF616A"  ;; 9  bright-red     — Aurora 0
     "#A3BE8C"  ;; 10 bright-green   — Aurora 4
     "#EBCB8B"  ;; 11 bright-yellow  — Aurora 3
     "#88C0D0"  ;; 12 bright-blue    — Frost 3 (= variables)
     "#C397D8"  ;; 13 bright-magenta — Lavender (= keywords)
     "#8FBCBB"  ;; 14 bright-cyan    — Frost 0
     "#ECEFF4")) ;; 15 bright-white  — Snow Storm 3 (default fg)
  (nano-set-magit-faces
   '(;; section
     (magit-section-heading           "#81A1C1" nil)       ;; salient / frost-2
     (magit-section-highlight         nil       "#434C5E") ;; subtle / polar-night-2
     (magit-section-heading-selection "#D08770" nil)       ;; popout / aurora-1
     ;; branches / refs
     (magit-branch-local              "#81A1C1" nil)       ;; frost-2
     (magit-branch-remote             "#A3BE8C" nil)       ;; = strings / aurora-4
     (magit-branch-remote-head        "#A3BE8C" nil)
     (magit-branch-current            "#88C0D0" nil)       ;; frost-3
     (magit-tag                       "#EBCB8B" nil)       ;; aurora-3
     (magit-hash                      "#677691" nil)       ;; faded
     (magit-dimmed                    "#677691" nil)
     ;; log
     (magit-log-author                "#BF616A" nil)       ;; aurora-0
     (magit-log-date                  "#677691" nil)
     (magit-log-graph                 "#4C566A" nil)       ;; polar-night-3
     ;; process
     (magit-process-ok                "#A3BE8C" nil)
     (magit-process-ng                "#BF616A" nil)
     ;; diffstat
     (magit-diffstat-added            "#A3BE8C" nil)
     (magit-diffstat-removed          "#BF616A" nil)
     ;; diff added/removed
     (magit-diff-added                "#A3BE8C" "#2B3B2B") ;; = string hues
     (magit-diff-added-highlight      "#A3BE8C" "#2F4A2F")
     (magit-diff-removed              "#BF616A" "#3B2020")
     (magit-diff-removed-highlight    "#BF616A" "#4A2B2B")
     ;; diff context
     (magit-diff-context              "#677691" nil)
     (magit-diff-context-highlight    "#8A9ABB" "#3B4252")
     ;; diff hunk headings
     (magit-diff-hunk-heading         "#D8DEE9" "#3B4252")
     (magit-diff-hunk-heading-highlight "#ECEFF4" "#4C566A")
     (magit-diff-hunk-heading-selection "#D08770" "#4C566A")
     ;; diff file headings
     (magit-diff-file-heading         "#ECEFF4" nil)
     (magit-diff-file-heading-highlight "#ECEFF4" "#434C5E")
     (magit-diff-file-heading-selection "#D08770" "#434C5E")
     ;; diff revision summary
     (magit-diff-revision-summary          "#D8DEE9" "#3B4252")
     (magit-diff-revision-summary-highlight "#ECEFF4" "#4C566A")
     ;; blame
     (magit-blame-heading   "#D8DEE9" "#3B4252")
     (magit-blame-margin    "#D8DEE9" "#3B4252")
     (magit-blame-highlight "#ECEFF4" "#434C5E")
     (magit-blame-dimmed    "#677691" nil)))
  (nano-set-dired-subtree-faces))

;; --- Command line theme chooser, initial theme ------------------------------
(add-to-list 'command-switch-alist '("-dark"  . nano-dark))
(add-to-list 'command-switch-alist '("-light" . nano-light))
(if (eq 'dark (frame-parameter nil 'background-mode)) (nano-light) (nano-dark))

;; --- Header & mode lines ----------------------------------------------------
(setq-default mode-line-format
              '(" " (:eval (when (mode-line-window-selected-p) (persp-mode-line)))))
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
                       (tasks '(t/modeline-segs))
                       (coords (format-mode-line "%c:%l ")))
                  (list
                   (propertize " " 'face (cdr prefix)  'display '(raise -0.25))
                   (propertize (car prefix) 'face (cdr prefix))
                   (propertize " " 'face (cdr prefix) 'display '(raise +0.25))
                   (propertize (format-mode-line " %b ") 'face 'nano-strong)
                   (when mode (propertize mode 'face `(:foreground ,(plist-get t-colors :hl))))
                   (propertize " " 'display `(space :align-to (- right ,(+ (length coords)
                                                                           (length (eval tasks))
                                                                           2))))
                   `(:eval ,tasks)
                   (propertize (format " %s" coords) 'face 'nano-faded)
                   ))))

;; --- Minibuffer setup -------------------------------------------------------
(defun nano-minibuffer--setup ()
  (setq truncate-lines t))
(add-hook 'minibuffer-setup-hook #'nano-minibuffer--setup)

(provide 'nano-t)
