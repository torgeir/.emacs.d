;;; -*- lexical-binding: t; -*-
(t/after evil

  (evil-define-text-object evil-org-outer-subtree (count &optional beg end type)
    "An Org subtree.  Uses code from `org-mark-subtree`"
    :type line
    (save-excursion
      ;; get to the top of the tree
      (org-with-limited-levels
       (cond ((org-at-heading-p) (beginning-of-line))
             ((org-before-first-heading-p) (user-error "Not in a subtree"))
             (t (outline-previous-visible-heading 1))))

      (decf count)
      (when count (while (and (> count 0) (org-up-heading-safe)) (decf count)))

      ;; extract the beginning and end of the tree
      (let ((element (org-element-at-point)))
        (list (org-element-property :end element)
              (org-element-property :begin element)))))

  (evil-define-text-object evil-org-inner-subtree (count &optional beg end type)
    "An Org subtree, minus its header and concluding line break.  Uses code from `org-mark-subtree`"
    :type line
    (save-excursion
      ;; get to the top of the tree
      (org-with-limited-levels
       (cond ((org-at-heading-p) (beginning-of-line))
             ((org-before-first-heading-p) (user-error "Not in a subtree"))
             (t (outline-previous-visible-heading 1))))

      (decf count)
      (when count (while (and (> count 0) (org-up-heading-safe)) (decf count)))

      ;; extract the beginning and end of the tree
      (let* ((element (org-element-at-point))
             (begin (save-excursion
                      (goto-char (org-element-property :begin element))
                      (next-line)
                      (point)))
             (end (save-excursion
                    (goto-char (org-element-property :end element))
                    (backward-char 1)
                    (point))))
        (list end begin))))

  (evil-define-text-object evil-org-outer-item (count &optional beg end type)
    :type line
    (let* ((struct (org-list-struct))
           (begin (org-list-get-item-begin))
           (end (org-list-get-item-end (point-at-bol) struct)))
      (if (or (not begin) (not end))
          nil
        (list begin end))))

  (evil-define-text-object evil-org-inner-item (count &optional beg end type)
    (let* ((struct (org-list-struct))
           (begin (progn (goto-char (org-list-get-item-begin))
                         (forward-char 2)
                         (point)))
           (end (org-list-get-item-end-before-blank (point-at-bol) struct)))
      (if (or (not begin) (not end))
          nil
        (list begin end))))

  (define-key evil-outer-text-objects-map "*" 'evil-org-outer-subtree)
  (define-key evil-inner-text-objects-map "*" 'evil-org-inner-subtree)
  (define-key evil-inner-text-objects-map "-" 'evil-org-inner-item)
  (define-key evil-outer-text-objects-map "-" 'evil-org-outer-item))

(provide 't-text-objects)