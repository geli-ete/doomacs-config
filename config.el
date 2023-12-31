;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Geli Ete"
      user-mail-address "emacsgeli@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Droid Sans" :size 13))
(setq doom-font (font-spec :family "Fira Code" :size 23 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Droid Sans" :size 24))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org-files/")
(setq org-agenda-files '("~/Dropbox/org-files/agenda/school.org"
                         "~/Dropbox/org-files/agenda/personal.org"
                         "~/Dropbox/org-files/agenda/projects.org"
                         "~/Dropbox/org-files/agenda/journal.org"
                         "~/Dropbox/org-files/agenda/tasks.org"))


;; Set `org-agenda-files'
;; `org-agenda' Setup After Package Load
(after! org
  (setq org-agenda-span 'week)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-into-drawer t)

  ;; Org Todo Keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; Org Agenda Custom Commands
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
          ("n" "Next Tasks" ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))
          ("W" "Work Tasks" tags-todo "+work-email")
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks") (org-agenda-max-todos 20)))
          ("w" "Workflow Status"
           ((todo "WAIT" ((org-agenda-overriding-header "Waiting on External")))
            (todo "REVIEW" ((org-agenda-overriding-header "In Review")))
            (todo "PLAN" ((org-agenda-overriding-header "In Planning") (org-agenda-todo-list-sublevels nil)))
            (todo "BACKLOG" ((org-agenda-overriding-header "Project Backlog") (org-agenda-todo-list-sublevels nil)))
            (todo "READY" ((org-agenda-overriding-header "Ready for Work")))
            (todo "ACTIVE" ((org-agenda-overriding-header "Active Projects")))
            (todo "COMPLETED" ((org-agenda-overriding-header "Completed Projects")))
            (todo "CANC" ((org-agenda-overriding-header "Cancelled Projects")))))))

  (after! org
    ;; Here we are setting `org-agenda-capture' as per our needs.
    ;; `%?' places the cursor after the template is filled.
    ;; `%U' inserts a timestamp.
    ;; `%a' includes a link to the current location (if capturing from within Emacs).
    ;; `%i' pulls in any selected text.
    ;; `:prepend' t adds new entries at the beginning of the file.
    (setq org-capture-templates
          '(
            ;; Add your templates here
            ("t" "Any Task" entry (file+olp+datetree "~/Dropbox/org-files/agenda/tasks.org")
             "* TODO %?\n  Created On: %U\n  Link: %a\n  Notes: %i" :prepend t)

            ("s" "School Task" entry (file+olp+datetree "~/Dropbox/org-files/agenda/school.org")
             "* TODO %?\n  Created On: %U\n  Link: %a\n  Notes: %i" :prepend t)

            ("p" "Personal Task" entry (file+olp+datetree "~/Dropbox/org-files/agenda/personal.org")
             "* TODO %?\n  Created On: %U\n  Link: %a\n  Notes: %i" :prepend t)

            ("r" "Project Task" entry (file+olp+datetree "~/Dropbox/org-files/agenda/projects.org")
             "* TODO %?\n  DEADLINE: %^t\n  Created On: %U\n  Link: %a\n  Notes: %i" :prepend t)

            ("j" "Journal Entry" entry (file+olp+datetree "~/Dropbox/org-files/agenda/journal.org")
             "* %?\n  Entered on %U\n Notes: %i" :prepend t :clock-in :clock-resume
             :empty-lines 1)

            ("b" "Birthday" entry (file "~/Dropbox/org-files/agenda/birthdays.org")
             "* %^t %?\n  Captured on: %U" :prepend t)

            ("h" "Habit" entry (file+olp+datetree "~/Dropbox/org-files/agenda/habits.org")
             "* TODO %?\n  SCHEDULED: <%<%Y-%m-%d %a .+1d>>\n  :PROPERTIES:\n  :STYLE: habit\n  :END:\n  Created On: %U Notes: %i" :prepend t)

            ))))

;; Setting `org-roam' directory using `org-roam-directory'.
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





;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
