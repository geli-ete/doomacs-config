;;; org-roam.el --- Doom Org-roam setup -*- lexical-binding: t; -*-

;; Base directory
(setq org-roam-directory (file-truename "~/Dropbox/org-files/roam"))

(require 'cl-lib)
(require 'seq)

;; Create subdirs (no shell needed)
(dolist (d '("daily" "projects" "refs" "permanent" "templates"))
  (make-directory (expand-file-name d org-roam-directory) t))

;; Stable links & basic toggles
(after! org
  (setq org-id-link-to-org-use-id t))

(after! org-roam
  (org-roam-db-autosync-mode 1)
  (setq org-roam-completion-everywhere t))

;; ---------------- Capture templates ----------------
(after! org-roam
  (setq org-roam-capture-templates
        '(
          ;; Permanent (atomic idea)
          ("d" "default (permanent)" plain "%?"
           :if-new (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :Permanent:\n")
           :unnarrowed t :empty-lines-before 1)

          ;; Programming language (permanent)
          ("l" "programming language" plain
           "* Characteristics\n\n- Family: %?\n- Inspired by:\n\n* References\n- \n"
           :if-new (file+head "permanent/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :Permanent:Lang:\n")
           :unnarrowed t :empty-lines-before 1)

          ;; Project → projects/
          ("p" "project" plain
           "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+category: ${title}\n#+filetags: :Project:\n")
           :unnarrowed t :empty-lines-before 1)

          ;; Reference / literature → refs/
          ("r" "reference / literature" plain
           "* Summary\n%?\n\n* Key Points\n- \n\n* Quotes\n- \"\" (p.)\n\n* Link\n- [[%^{URL}][Source]]\n"
           :if-new (file+head "refs/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :Ref:Lit:\n")
           :unnarrowed t)

          ;; AI digest → refs/
          ("a" "AI digest" plain
           "* Question\n%^{Prompt}\n\n* TL;DR\n- \n\n* Claims to verify\n- [ ] \n\n* Sources\n- \n\n* Decisions / Next steps\n- \n"
           :if-new (file+head "refs/%<%Y%m%d%H%M%S>-ai-${slug}.org"
                              "#+title: ${title}\n#+filetags: :AI:Ref:\n")
           :unnarrowed t)
          ))

  ;; Dailies
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(
          ;; Use this once per new day to seed the page
          ("d" "new day (seed full skeleton)" entry
           ""
           :if-new (file+head
                    "%<%Y-%m-%d>.org"
                    "#+title: %<%Y-%m-%d>\n
* Plan
- Top 3 -> [ ] 1  [ ] 2  [ ] 3

* Ideas to Promote

* Tasks

* Journal

* Meetings
- [ ] %^{Meeting title}  %^T
  - Notes:

* Highlights
- Win:
- Challenge:
- One-line summary:

* Review (evening)
- What moved forward?
- Tomorrow's first action:
")
           :immediate-finish t)

          ;; Quick journal line under today's Journal
          ("j" "journal (timestamped line)" entry
           "* %<%I:%M %p>: %?\n"
           :if-new (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n"
                                  ("Journal")))

          ;; Quick task under today's Tasks
          ("t" "task (into today's Tasks)" entry
           "** TODO %?\n:CREATED: %U\n"
           :if-new (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n"
                                  ("Tasks")))

          ;; Quick idea under today's Ideas to Promote
          ("i" "idea to promote" entry
           "* %?\n"
           :if-new (file+head+olp "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n"
                                  ("Ideas to Promote")))
          ))


  (require 'org-roam-dailies))

;; ---------------- Helpers ----------------
(defun org-roam-node-insert-immediate (arg &rest args)
  "Insert an Org-roam node and finish capture immediately."
  (interactive "P")
  (let* ((orig org-roam-capture-templates)
         (immediate (mapcar (lambda (tpl) (append tpl '(:immediate-finish t))) orig)))
    (let ((org-roam-capture-templates immediate))
      (apply #'org-roam-node-insert arg args))))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node) (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (when (fboundp 'org-roam-node-list)
    (mapcar #'org-roam-node-file
            (seq-filter (my/org-roam-filter-by-tag tag-name)
                        (org-roam-node-list)))))

;; ---------------- Agenda integration ----------------
(defun my/org-roam-refresh-agenda-list ()
  "Merge :Project: files into `org-agenda-files`."
  (interactive)
  (let* ((project-files (my/org-roam-list-notes-by-tag "Project"))
         (pf (mapcar #'file-truename project-files))
         (af (mapcar #'file-truename org-agenda-files))
         (merged (delete-dups (append af pf))))
    (when merged (setq org-agenda-files merged))))

(my/org-roam-refresh-agenda-list)
(run-at-time "00:01" 86400 #'my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (file-truename (buffer-file-name))))))

(defun my/org-roam-find-project ()
  "Find an existing :Project: or create a new one."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (let ((org-roam-capture-templates
         '(("p" "project" plain
            "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
            :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n#+category: ${title}\n#+filetags: :Project:\n")
            :unnarrowed t))))
    (org-roam-node-find nil nil (my/org-roam-filter-by-tag "Project"))))

;; ---------------- Inbox & tasks ----------------
(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture-
   :node (org-roam-node-create)
   :templates
   '(("i" "inbox" plain "* %?\n:CREATED: %U\n"
      :if-new (file+head "inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (org-roam-capture-
   :node (org-roam-node-read nil (my/org-roam-filter-by-tag "Project"))
   :templates
   '(("p" "project task" plain "** TODO %?"
      :if-new (file+head+olp "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+category: ${title}\n#+filetags: :Project:\n"
                             ("Tasks"))))))

;; DONE -> refile to today's Daily under Tasks
(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t)
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org"
                                   "#+title: %<%Y-%m-%d>\n"
                                   ("Tasks")))))
        today-file pos)
    (save-window-excursion
      (org-roam-dailies-capture-today)
      (setq today-file (buffer-file-name))
      (setq pos (point))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (and (boundp 'org-state) (string= org-state "DONE"))
              (my/org-roam-copy-todo-to-today))))

;; ---------------- Graph & UI ----------------
(after! org-roam
  (setq org-roam-graph-executable (or (executable-find "dot")
                                      "/opt/homebrew/bin/dot")
        org-roam-graph-viewer (lambda (file) (browse-url file))
        org-roam-graph-filetypes '("svg")
        org-roam-graph-shorten-titles t
        org-roam-graph-title-max-length 40
        org-roam-graph-extra-config
        '(("rankdir" . "TB")
          ("concentrate" . "true")
          ("overlap" . "false")
          ("splines" . "true")
          ("ranksep" . "0.6")
          ("nodesep" . "0.35")
          ("ratio" . "compress"))
        org-roam-graph-node-extra-config
        '(("shape" . "box")
          ("style" . "filled")
          ("fontsize" . "10")
          ("fillcolor" . "#eeeeee")
          ("color" . "#c9c9c9"))
        org-roam-graph-edge-extra-config
        '(("color" . " #999999")
          ("penwidth" . "1"))
        org-roam-graph-exclude-matcher
        "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'\\|\\`https?://"))

(defun +my/org-roam-graph-here (&optional depth)
  (interactive "p")
  (let ((current-prefix-arg (or depth 1)))
    (call-interactively #'org-roam-graph)))
(defun +my/org-roam-graph-1hop () (interactive) (+my/org-roam-graph-here 1))
(defun +my/org-roam-graph-2hop () (interactive) (+my/org-roam-graph-here 2))

(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; End of file
