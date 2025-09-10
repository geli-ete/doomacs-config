;;; custom-keybindings.el -*- lexical-binding: t; -*-

;; If this file lives in ~/.doom.d/, make sure config.el has:
;;   (load! "custom-keybindings")

;; --- Helper commands for org-roam graph ----------------------------------
;; Make tiny interactive commands so keybindings never call an undefined lambda
;; and so which-key shows nice names.

(defun +my/org-roam-graph-here (&optional depth)
  "Graph the neighborhood of the current Org-roam note. DEPTH defaults to 1."
  (interactive "p")
  (let ((current-prefix-arg (or depth 1)))
    (call-interactively #'org-roam-graph)))

(defun +my/org-roam-graph-1hop () (interactive) (+my/org-roam-graph-here 1))
(defun +my/org-roam-graph-2hop () (interactive) (+my/org-roam-graph-here 2))

;; --- Your existing bindings (kept) ---------------------------------------
(map! :leader
      :desc "Personal Journal" "J" #'my-personal-journal
      :desc "Central Hub"      "F" #'my-favorite-file
      :desc "Study Log"        "S" #'my-study-log-file)

(after! org
  (map! :map org-mode-map
        "C-c C-e" #'efs/ielm-send-line-or-region))

(global-set-key (kbd "C-c E") #'efs/toggle-ielm)

(after! org
  (map! :map org-mode-map
        :localleader
        "I" #'org-id-get-create
        "C" #'org-id-copy))

;; Tabs
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

;; Comment / copy helpers
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

;; Scrolling other window
(global-set-key (kbd "C-c j") #'scroll-other-window-down-one-line)
(global-set-key (kbd "C-c k") #'scroll-other-window-up-one-line)

;; Native completion + org narrow + undo-fu
(global-set-key (kbd "M-/")    #'dabbrev-expand)
(global-set-key (kbd "M-C-/")  #'dabbrev-completion)
(global-set-key (kbd "C-c u")  #'org-toggle-narrow-to-subtree)
(global-set-key (kbd "C-z")    #'undo-fu-only-undo)
(global-set-key (kbd "C-S-z")  #'undo-fu-only-redo)

;; Org-roam keys (leader: SPC)
(map! :leader
      (:prefix ("n r" . "org-roam")
       :desc "Capture Inbox"           "i" #'my/org-roam-capture-inbox
       :desc "Find/Create Project"     "P" #'my/org-roam-find-project
       :desc "Capture Project Task"    "T" #'my/org-roam-capture-task
       :desc "Insert Immediate"        "I" #'org-roam-node-insert-immediate
       :desc "Graph (this note, 1-hop)" "1" #'+my/org-roam-graph-1hop
       :desc "Graph (this note, 2-hop)" "2" #'+my/org-roam-graph-2hop
       :desc "Graph (prompt/full)"      "g" #'org-roam-graph))
