;;; custom-keybindings.el -*- lexical-binding: t; -*-
;; Using `general' keybindings.
;;(map! :ne "M-o" #'treemacs-select-window)

;; (map! :mode org-mode
;;       :leader
;;       :desc "Org capture" "X" #'org-capture)

(map! :leader
      :desc "Central Hub" "F" #'my-favorite-file)

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
;; (map! :leader
;;       :desc "Insert src block"
;;       "i s" #'my-insert-src-block)
