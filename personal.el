;;; personal.el -*- lexical-binding: t; -*-

(defun my-favorite-file ()
  "Open my favorite file."
  (interactive)
  (find-file "~/Dropbox/org-files/roam/20231227192222-dashboard.org"))

(defun my-study-log-file ()
  "Open my study log!"
  (interactive)
  (find-file "/home/geli/240115-geli/study-project/study-log-24jan.org"))

(defun append-region-to-scratch (start end)
  "Append the selected region to the *scratch* buffer."
  (interactive "r")  ;; This makes the function use the current region
  (let ((selected-text (buffer-substring start end)))  ;; Extracts the region's text
    (with-current-buffer "*scratch*"  ;; Switches context to the *scratch* buffer
      (goto-char (point-max))  ;; Moves the cursor to the end of the *scratch* buffer
      (insert "\n" selected-text))))  ;; Inserts the selected text at the end

;; copy functions
(defun copy-line-or-region-up ()
  "Copy the current line or region upwards."
  (interactive)
  (if (use-region-p)
      (progn
        ;; Copy and paste region
        (kill-ring-save (region-beginning) (region-end))
        (goto-char (region-beginning))
        (yank)
        (exchange-point-and-mark))
    ;; Copy and paste line
    (progn
      (kill-ring-save (line-beginning-position) (line-beginning-position 2))
      (beginning-of-line)
      (yank))))

(defun copy-line-or-region-down ()
  "Copy the current line or region downwards."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        ;; Copy and paste region
        (kill-ring-save start end)
        (goto-char end)
        (newline)
        (yank)
        (exchange-point-and-mark))
    ;; Copy and paste line without adding an extra newline
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (kill-ring-save start end)
      (end-of-line)
      (newline)
      (yank)
      ;; Move cursor to the beginning of the original line
      (goto-char start))))

;; function to toggle sly mrepl
(defun toggle-sly-mrepl ()
  "Toggle the visibility of the sly-mrepl buffer. Remove window split if necessary."
  (interactive)
  (let ((repl-buffer (sly-mrepl--find-buffer)))
    (if (and repl-buffer (get-buffer-window repl-buffer))
        (if (= 1 (length (window-list)))
            (bury-buffer repl-buffer)
          (delete-window (get-buffer-window repl-buffer)))
      (when repl-buffer
        (pop-to-buffer repl-buffer)))))

;; Copy a line
(defun copy-current-line ()
  "Copy the current line to the Emacs kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-beginning-position 2))
  (message "Line copied"))

(defun my-insert-src-block ()
  "Insert an Org mode source block by selecting a language from a predefined list."
  (interactive)
  (let ((languages '("zig" "python" "emacs-lisp" "shell" "R" "java" "C++" "javascript"))
        lang)
    (setq lang (completing-read "Choose language: " languages))
    (if (equal lang "python")
        (insert "#+begin_src python :results output\n")
      (insert (format "#+begin_src %s\n" lang)))
    (save-excursion ;; Use save-excursion to return the cursor after inserting the end block
      (newline)
      (insert "#+end_src"))
    (newline)))

;;; Scroll Up or Down other window by one line
;; Scroll up one line
(defun scroll-other-window-up-one-line ()
  "Scroll the other window up by one line."
  (interactive)
  (scroll-other-window '-1))
;; Scroll down one line
(defun scroll-other-window-down-one-line ()
  "Scroll the other window down by one line."
  (interactive)
  (scroll-other-window 1))

(defun extract-entry-data (entry)
  "Extract data from ENTRY for JSON."
  (let ((title (org-element-property :raw-value entry))
        (deadline (org-entry-get (org-element-property :begin entry) "DEADLINE" t))
        (scheduled (org-entry-get (org-element-property :begin entry) "SCHEDULED" t)))
    `((title . ,title) (deadline . ,deadline) (scheduled . ,scheduled))))

;; Setup for assembly language
(setq auto-mode-alist (cons '("\\.hla\\'" . asm-mode) auto-mode-alist))

;; Enable global word wrap
(global-visual-line-mode 1)

;;; ielm setup
;; Function to send line or region to ielm
(defun efs/ielm-send-line-or-region ()
  (interactive)
  (unless (use-region-p)
    (forward-line 0)
    (set-mark-command nil)
    (forward-line 1))
  (backward-char 1)
  (let ((text (buffer-substring-no-properties (region-beginning)
                                              (region-end))))
    (with-current-buffer (get-buffer-create "*ielm*")
      (insert text)
      (ielm-send-input))
    (deactivate-mark)))

;; Function to show ielm
(defun efs/show-ielm ()
  (interactive)
  (select-window (split-window-vertically -10))
  (ielm)
  (text-scale-set 1))

;;; Add this to your config.el in your Doom Emacs directory

(defun efs/toggle-ielm ()
  "Toggle ielm window."
  (interactive)
  (if (and (get-buffer "*ielm*")               ;; Check if "*ielm*" buffer exists
           (get-buffer-window "*ielm*" 'visible)) ;; Check if visible
      (if (eq (current-buffer) (get-buffer "*ielm*"))
          (delete-window) ;; If ielm is current buffer, delete the window
        (pop-to-buffer "*ielm*")) ;; Else switch to the ielm buffer
    (progn
      (select-window (split-window-vertically -10))
      (ielm)
      (text-scale-set 1))))

;; (after! mu4e

;;   ;; send email via mu4e for
;;   (defun send-email-via-mu4e (recipient subject body)
;;     "For sending mail from mu4e."
;;     (mu4e-compose-mail
;;      :to recipient
;;      :subject subject
;;      :body body))
;;   )

;; (send-email-via-mu4e "geliete@proton.me" "Test Subject" "Test Body")'
;; (process-org-entries-to-json-and-write-to-file)

(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'org-display-inline-images)
