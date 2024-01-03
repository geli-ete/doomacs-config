;;; org-agenda.el -*- lexical-binding: t; -*-
(setq org-directory "~/Dropbox/org-files/")
(setq org-agenda-files '("~/Dropbox/org-files/agenda/school.org"
                         "~/Dropbox/org-files/agenda/personal.org"
                         "~/Dropbox/org-files/agenda/projects.org"
                         "~/Dropbox/org-files/agenda/journal.org"
                         "~/Dropbox/org-files/agenda/tasks.org"))


;; Set `org-agenda-files'
;; `org-agenda' Setup After Package Load
(after! org
  (setq org-hide-emphasis-markers t)
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
