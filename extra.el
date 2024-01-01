;;; extra.el -*- lexical-binding: t; -*-
;; Remove default doom banner at startup
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-banner)

(require 'c3po)
(setq c3po-api-key "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")

;; Extra Configurations
(repeat-mode)
(auto-revert-mode) ;; For auto refresh of changed buffer

;; Org-Emphasis is mostly hidden except when your cursor in on that word.
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

;; adjusts the size of Emacs windows automatically using the golden ratio principle
(use-package golden-ratio
  :config
  (golden-ratio-mode 1)
  ;; Optional configurations go here. For example:
  ;; (setq golden-ratio-adjust-factor .8
  ;;       golden-ratio-wide-adjust-factor .8)
  )

(use-package vertico
  :init
  (vertico-mode)
  :config
  (use-package vertico-posframe
    :config
    (vertico-posframe-mode 1)))
