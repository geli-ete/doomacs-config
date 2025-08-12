;;; extra.el -*- lexical-binding: t; -*-
;; Remove default doom banner at startup
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-banner)

;; SLY (Sylvester the Cat's Common Lisp IDE)
(after! sly
  ;; Set the default Lisp interpreter to a Roswell-managed implementation
  (setq inferior-lisp-program "ros -Q run"))

;; (require 'c3po)
;; (setq c3po-api-key "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")

;; Extra Configurations
(repeat-mode)
;;(auto-revert-mode) ;; For auto refresh of changed buffer
(global-auto-revert-mode t)

;; Org-Emphasis is mostly hidden except when your cursor in on that word.
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

(use-package! vertico
  :init
  (vertico-mode)
  :config
  (use-package vertico-posframe
    :config
    (vertico-posframe-mode 1)))

(use-package! org-noter
  :after org
  :config
  ;; Your org-noter configuration goes here
  )


