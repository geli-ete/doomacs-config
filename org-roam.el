;;; org-roam.el -*- lexical-binding: t; -*-

(setq org-roam-directory (file-truename "~/Dropbox/org-files/roam"))
;; Org-Roam Setup After Package Load
(after! org-roam
  ;; Define Org-Roam Capture Templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
          ("l" "programming language" plain
           "* Characteristics\n\n - Family: %?\n- Inspired by: \n\n* References:\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("b" "book notes" plain
           (file "~/Dropbox/RoamNotes/Templates/BookNoteTemplate.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "project" plain
           "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
           :unnarrowed t)          ;; Add more templates as needed
          ))
  ;; Setting Up Daily Notes
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  )

;; Custom functions for Org-roam
(require 'cl-lib)

(defun org-roam-node-insert-immediate (arg &rest args)
  "Insert an Org-roam node immediately with given ARG and ARGS."
  (interactive "P")
  (let ((org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert arg args)))

(defun my/org-roam-refresh-agenda-list ()
  "Update `org-agenda-files` to include Org-roam files tagged with 'Project'."
  (interactive)
  (let ((project-files (my/org-roam-list-notes-by-tag "Project")))
    (when project-files
      (setq org-agenda-files (append org-agenda-files project-files)))))

(defun my/org-roam-daily-refresh-agenda-list ()
  "Refresh agenda list once daily."
  (run-at-time "00:01" 86400 'my/org-roam-refresh-agenda-list))

;; Call the function to set up the daily refresh.
(my/org-roam-daily-refresh-agenda-list)

(defun my/org-roam-filter-by-tag (tag-name)
  "Return a function to filter Org-roam nodes by a given TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "Return a list of Org-roam file paths that contain the given TAG-NAME."
  (when (fboundp 'org-roam-node-list)
    (mapcar #'org-roam-node-file
            (seq-filter (lambda (node) (member tag-name (org-roam-node-tags node)))
                        (org-roam-node-list)))))
(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the capture was not aborted."
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (let ((org-roam-capture-templates
         '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
            :unnarrowed t))))
    (org-roam-node-find nil nil (my/org-roam-filter-by-tag "Project"))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (org-roam-capture- :node (org-roam-node-read nil (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t)
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        today-file pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point))
      (unless (equal (file-truename today-file) (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today)))))
