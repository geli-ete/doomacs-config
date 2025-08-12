;; Set the Org-Roam directory
(setq org-roam-directory (file-truename "~/Dropbox/org-files/roam"))

(after! org-roam
  ;; Capture Templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
          ;; Programming Language Template
          ("l" "programming language" plain
           "* Characteristics\n\n - Family: %?\n- Inspired by: \n\n* References:\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ;; Book Notes Template
          ("b" "book notes" plain
           (file "~/Dropbox/RoamNotes/Templates/BookNoteTemplate.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ;; Project Template
          ("p" "project" plain
           "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
           :unnarrowed t)))

  ;; Daily Notes
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

  ;; Enable autosync for the Org-Roam database
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

;; Custom Functions for Org-Roam
(require 'cl-lib)

(defun org-roam-node-insert-immediate (arg &rest args)
  "Insert an Org-roam node immediately with given ARG and ARGS."
  (interactive "P")
  (let ((org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert arg args)))

;; Refresh Org-Roam Agenda List Daily
(defun my/org-roam-refresh-agenda-list ()
  "Update `org-agenda-files` to include Org-roam files tagged with 'Project'."
  (interactive)
  (let ((project-files (my/org-roam-list-notes-by-tag "Project")))
    (when project-files
      (setq org-agenda-files (append org-agenda-files project-files)))))

(run-at-time "00:01" 86400 'my/org-roam-refresh-agenda-list)

;; Filter Org-Roam Notes by Tag
(defun my/org-roam-filter-by-tag (tag-name)
  "Return a function to filter Org-roam nodes by TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "Return a list of Org-roam file paths containing TAG-NAME."
  (when (fboundp 'org-roam-node-list)
    (mapcar #'org-roam-node-file
            (seq-filter (my/org-roam-filter-by-tag tag-name)
                        (org-roam-node-list)))))

;; Add Projects to Org-Agenda Files on Capture Finalization
(defun my/org-roam-project-finalize-hook ()
  "Add the captured project file to `org-agenda-files' if capture was not aborted."
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  "Find an existing Org-Roam project or create a new one."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (let ((org-roam-capture-templates
         '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
            :unnarrowed t))))
    (org-roam-node-find nil nil (my/org-roam-filter-by-tag "Project"))))

;; Capture Inbox and Task Templates
(defun my/org-roam-capture-inbox ()
  "Capture to Org-Roam Inbox."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  "Capture a task within a project."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (org-roam-capture- :node (org-roam-node-read nil (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

;; Automatically Copy TODOs to Today's Daily Note on Completion
(defun my/org-roam-copy-todo-to-today ()
  "Refile completed TODOs to today's daily note."
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

  ;; Add to after-todo hook to execute upon task completion
  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today)))))
