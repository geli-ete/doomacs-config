;;; agenda-scheduler.el -*- lexical-binding: t; -*-
(require 'json)

(require 'json)

(require 'json)

(defun export-org-data-to-json ()
  (interactive)
  (let ((source-file "~/Dropbox/org-files/agenda/school.org")
        (output-file "~/Dropbox/org-files/external/org-agenda.json")
        (tasks '()))
    (with-temp-buffer
      (insert-file-contents source-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(TODO\\|DONE\\) \\(.*?\\)\\(\\s-*\\[:.*?:\\]\\)?\\s-*\\n" nil t)
        (let* ((status (list (match-string 1)))
               (heading (match-string 2))
               (tags (and (match-string 3) (split-string (match-string 3) ":" t)))
               (additional-info '()))
          ;; Extract additional information such as SCHEDULED, DEADLINE, etc.
          (while (re-search-forward "SCHEDULED: <\\(.*?\\)>\\|DEADLINE: <\\(.*?\\)>\\|Created On: \\[\\(.*?\\)\\]\\|Link: \\(.*?\\)\\n\\|Notes: \\(.*?\\)\\(\\n\\*+ \\|\\'\\)" nil t)
            (push (cond ((match-string 1) `(:scheduled ,(match-string 1)))
                        ((match-string 2) `(:deadline ,(match-string 2)))
                        ((match-string 3) `(:created_on ,(match-string 3)))
                        ((match-string 4) `(:link ,(match-string 4)))
                        ((match-string 5) `(:notes ,(match-string 5)))) additional-info))
          ;; Combine all data into a task
          (push `(:agenda-data [(,:status ,status :tags ,tags :heading ,heading ,@additional-info)]) tasks)))
      (with-temp-file output-file
        (insert (json-encode `(:tasks ,@tasks :something-else []))))
      (message "Org data exported to %s" output-file))))

(export-org-data-to-json)

;; ((source-file "~/Dropbox/org-files/agenda/school.org")
;;  (output-file "~/Dropbox/org-files/external/org-agenda.json")
;;  (data (list 'tasks nil)))
