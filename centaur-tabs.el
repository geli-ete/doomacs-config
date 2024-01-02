;;; centaur-tabs.el -*- lexical-binding: t; -*-

(use-package! centaur-tabs
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'under
        centaur-tabs-show-count nil
        x-underline-at-descent-line t
        centaur-tabs-left-edge-margin nil)
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/"
        uniquify-buffer-name-style 'forward)

  (defun centaur-tabs-buffer-groups ()
    "Control buffer groups for centaur tabs."
    (list (cond
           ((or (string-equal "*" (substring (buffer-name) 0 1))
                (derived-mode-p 'magit-mode))
            "Emacs")
           ((derived-mode-p 'prog-mode)
            "Editing")
           ((derived-mode-p 'dired-mode)
            "Dired")
           ((derived-mode-p 'helpful-mode 'help-mode)
            "Help")
           ((derived-mode-p 'org-mode)
            "OrgMode")
           (t
            (centaur-tabs-get-group-name (current-buffer))))))

  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode))

;; Centaur Tabs Configuration
;; (after! centaur-tabs
;;   (setq centaur-tabs-style "rounded"  ; Set the style of the tabs
;;         centaur-tabs-height 32  ; Set the height of the tabs
;;         centaur-tabs-set-icons t  ; Enable icons in the tabs
;;         centaur-tabs-set-modified-marker t  ; Show a marker for modified buffers
;;         centaur-tabs-show-navigation-buttons t  ; Show navigation buttons on the tabs
;;         centaur-tabs-set-bar 'under  ; Set the position of the selection bar
;;         x-underline-at-descent-line t)
;;   (centaur-tabs-headline-match)  ; Enable headline matching for grouping tabs
;;   ;; Customize how tabs are grouped
;;   (setq centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups))
