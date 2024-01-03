;;; org-mode.el -*- lexical-binding: t; -*-

;; Configure org-superstar
;;; Add this to your config.el in your Doom Emacs directory

(after! org
  ;; Change bullet symbols for lists
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1)))

  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  ;; Set custom heading sizes
  (custom-set-faces!
    `(org-level-1 :inherit outline-1 :font "Cantarell" :weight regular :height 0.85)
    `(org-level-2 :inherit outline-2 :font "Cantarell" :weight regular :height 1.09)
    `(org-level-3 :inherit outline-3 :font "Cantarell" :weight regular :height 1.05)
    `(org-level-4 :inherit outline-4 :font "Cantarell" :weight regular :height 1.1)
    `(org-level-5 :inherit outline-5 :font "Cantarell" :weight regular :height 1.1)
    `(org-level-6 :inherit outline-6 :font "Cantarell" :weight regular :height 1.1)
    `(org-level-7 :inherit outline-7 :font "Cantarell" :weight regular :height 1.1)
    `(org-level-8 :inherit outline-8 :font "Cantarell" :weight regular :height 1.1))
  ;; Configure fixed-pitch faces
  (custom-set-faces!
    `(org-table :inherit fixed-pitch)
    `(org-formula :inherit fixed-pitch)
    `(org-code :inherit (shadow fixed-pitch))
    `(org-indent :inherit (org-hide fixed-pitch))
    `(org-verbatim :inherit (shadow fixed-pitch))
    `(org-special-keyword :inherit (font-lock-comment-face fixed-pitch))
    `(org-meta-line :inherit (font-lock-comment-face fixed-pitch))
    `(org-checkbox :inherit fixed-pitch))

  ;;Customize visual-fill-column
  (use-package! visual-fill-column
    :hook (org-mode . dw/org-mode-visual-fill))

  (defun dw/org-mode-visual-fill ()
    (setq visual-fill-column-width 120
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  )
;; Customize org-superstar
(setq org-superstar-headline-bullets-list '("✸" "✿" "◉" "○" "▶"))
(setq org-superstar-prettify-item-bullets t)
