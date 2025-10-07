;;; org-mode.el -*- lexical-binding: t; -*-

(after! org
  ;; Configure tags alignment (optional)
  ;; (setq org-tags-column 80)
  ;; (add-hook 'window-configuration-change-hook
  ;;           (lambda () (setq org-tags-column (- (window-width) 20))))

  ;; Org mode setup function
  (defun geli/org-mode-setup ()
    (display-line-numbers-mode 0)
    (org-superstar-mode 1))

  ;; Enable `geli/org-mode-setup` on `org-mode-hook`
  (add-hook 'org-mode-hook #'geli/org-mode-setup)

  ;; Configure org-superstar
  (setq org-superstar-prettify-item-bullets t
        org-superstar-headline-bullets-list '(?✸ ?✿ ?◉ ?○ ?▶)
        org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➤) (?- . ?—))
        org-superstar-leading-bullet ?\s
        org-superstar-cycle-headline-bullets nil
        org-hide-leading-stars t)


  ;; Customize heading levels with specific fonts
  (custom-set-faces!
    `(org-level-1 :inherit outline-1 :font "Cantarell" :weight regular :height 1.10)
    `(org-level-2 :inherit outline-2 :font "Cantarell" :weight regular :height 1.05)
    `(org-level-3 :inherit outline-3 :font "Cantarell" :weight regular :height 1.00)
    `(org-level-4 :inherit outline-4 :font "Cantarell" :weight regular :height 1.1)
    `(org-level-5 :inherit outline-5 :font "Cantarell" :weight regular :height 1.1)
    `(org-level-6 :inherit outline-6 :font "Cantarell" :weight regular :height 1.1)
    `(org-level-7 :inherit outline-7 :font "Cantarell" :weight regular :height 1.1)
    `(org-level-8 :inherit outline-8 :font "Cantarell" :weight regular :height 1.1))

  ;; Fixed-pitch faces for specific Org elements
  (custom-set-faces!
    `(org-table :inherit fixed-pitch)
    `(org-formula :inherit fixed-pitch)
    `(org-code :inherit (shadow fixed-pitch))
    `(org-indent :inherit (org-hide fixed-pitch))
    `(org-verbatim :inherit (shadow fixed-pitch))
    `(org-special-keyword :inherit (font-lock-comment-face fixed-pitch))
    `(org-meta-line :inherit (font-lock-comment-face fixed-pitch))
    `(org-checkbox :inherit fixed-pitch))

  ;; Visual fill column configuration
  (use-package! visual-fill-column
    :hook (org-mode . dw/org-mode-visual-fill)
    :config
    (defun dw/org-mode-visual-fill ()
      (setq visual-fill-column-width 120
            visual-fill-column-center-text t)
      (visual-fill-column-mode 1)))
  )
