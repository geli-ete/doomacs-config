;;; custom-keybindings.el -*- lexical-binding: t; -*-
;; Using `general' keybindings.
;;(map! :ne "M-o" #'treemacs-select-window)

;; (map! :mode org-mode
;;       :leader
;;       :desc "Org capture" "X" #'org-capture)

(map! :leader
      :desc "Central Hub" "F" #'my-favorite-file
      :desc "Study Log" "J" #'my-study-log-file)
;; Set keybindings for Org mode in Doom Emacs
(after! org
  (map! :map org-mode-map
        "C-c C-e" #'efs/ielm-send-line-or-region
        ;;"C-c E"   #'efs/show-ielm)
        ))

;; Global keybinding
(map! "C-c E" #'efs/toggle-ielm)

(after! org
  (map! :map org-mode-map
        :localleader
        "I" #'org-id-get-create
        "C" #'org-id-copy))

;;Tab Management Keybindings
(map! :leader
      (:prefix "t"
               "n" #'centaur-tabs-forward
               "p" #'centaur-tabs-backward
               "N" #'centaur-tabs-forward-group
               "P" #'centaur-tabs-backward-group
               "c" #'centaur-tabs--create-new-tab
               "k" #'centaur-tabs-kill-other-tabs-in-current-group
               "K" #'centaur-tabs-kill-all-tabs-in-current-group
               "r" #'centaur-tabs-switch-group
               "R" #'centaur-tabs-rename-group
               "g" #'centaur-tabs-group-by-projectile-project
               "m" #'centaur-tabs-move-current-tab-to-left
               "M" #'centaur-tabs-move-current-tab-to-right))

(map! :leader
      (:prefix "c"
               "c" #'comment-or-uncomment-region
               "u" #'uncomment-region
               "p" #'copy-line-or-region-up
               "P" #'copy-line-or-region-down
               "y" #'copy-current-line))

(map! :leader
      (:prefix "m"
               "\"" #'toggle-sly-mrepl))

(map! :leader
      :desc "Insert src block"
      "i s" #'my-insert-src-block)

;; Key bindings for scroll other window by one line
(global-set-key (kbd "C-c j") 'scroll-other-window-down-one-line)
(global-set-key (kbd "C-c k") 'scroll-other-window-up-one-line)

;; native completion
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "M-C-/") 'dabbrev-completion)

(global-set-key (kbd "C-c u") 'org-toggle-narrow-to-subtree)
