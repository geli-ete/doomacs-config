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


(after! tramp
  (setq tramp-default-method "ssh"
        tramp-verbose 1
        remote-file-name-inhibit-cache nil
        tramp-use-ssh-controlmaster-options t))

;; Make Treemacs less chatty on refresh (minor polish)
(with-eval-after-load 'treemacs
  (setq treemacs-no-pulse-on-refresh t))

;; Exec from shell
(use-package! exec-path-from-shell
  :init
  (exec-path-from-shell-copy-envs '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "LANG" "LC_ALL")))

;; yasnippets
(after! yasnippet
  (setq yas-snippet-dirs '("~/.doom.d/snippets" "~/.emacs.d/snippets")))

;; In your mac's ~/.doom.d/config.el
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path) ; include remote user's PATH
  (add-to-list 'tramp-remote-path "~/.local/bin")
  (add-to-list 'tramp-remote-path "~/go/bin"))

;; Disable VC backends for remote files (big win)
(add-hook 'find-file-hook
          (defun +no-vc-on-remote-h ()
            (when (file-remote-p default-directory)
              (setq-local vc-handled-backends nil))))

;; Projectile: avoid heavy indexing remotely
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-verbose t
        projectile-auto-discover nil     ; don’t auto-scan drives
        projectile-enable-caching t))

;; Dired: don’t auto-revert remote buffers (cuts background pings)
(add-hook 'dired-mode-hook
          (defun +no-auto-revert-remote-dired-h ()
            (when (file-remote-p default-directory)
              (auto-revert-mode -1))))

;; ~/.doom.d/config.el (on your Mac)
(after! tramp
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/home/geliete/.nvm/versions/node/v20.19.0/bin"))
