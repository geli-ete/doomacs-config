;;; custom-keybindings.el -*- lexical-binding: t; -*-
;; Using `general' keybindings.
(map! :ne "M-o" #'treemacs-select-window)

;; (map! :mode org-mode
;;       :leader
;;       :desc "Org capture" "X" #'org-capture)

(map! :leader
      :desc "Central Hub" "F" #'my-favorite-file)
