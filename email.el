;;; email.el -*- lexical-binding: t; -*-

;; (after! org
;;   (org-agenda-to-appt)  ; Convert org-agenda to appointments
;;   (setq appt-disp-window-function 'my-custom-appt-notification-function)

;;  )
(after! mu4e

  (setq mu4e-root-maildir "~/Mail"             ;; top-level Maildir
        mu4e-get-mail-command "mbsync -a"  ;; sync command
        mu4e-update-interval 300          ;; update every 5 minutes
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-completing-read-function #'completing-read
        mu4e-use-fancy-chars t
        mu4e-search-skip-duplicates t
        mu4e-change-filenames-when-moving t
        ;; more settings...
        )

  ;; Set up contexts if you use them
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Personal"
             :match-func (lambda (msg) (when msg
                                         (string-prefix-p "/personal" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address . "emacs.geli@gmail.com")
                     (mu4e-drafts-folder . "/personal/Drafts")
                     (mu4e-sent-folder . "/personal/Sent")
                     (mu4e-refile-folder . "/personal/Archive")
                     (mu4e-trash-folder . "/personal/Trash")))
           ;; Additional contexts...
           ))

  ;; Mail sending settings (use smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user "emacs.geli@gmail.com")  ; Replace with your Gmail address
  )

(defun send-email (to subject body)
  "Send an email."
  (require 'smtpmail)
  (let ((message-signature nil))  ; Disable signature if you don't want it in automated emails
    (with-temp-buffer
      (insert "To: " to "\n")
      (insert "Subject: " subject "\n")
      (insert "From: emacs.geli@gmail.com\n")  ; Your email address
      (insert "\n")
      (insert body)
      (smtpmail-send-it))))

;; Example usage:
;; (send-email "geliete@proton.me" "Finally" "This is a test body for test man.")
