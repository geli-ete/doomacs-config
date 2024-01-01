;;; personal.el -*- lexical-binding: t; -*-

(defun my-favorite-file ()
  "Open my favorite file."
  (interactive)
  (find-file "~/Dropbox/org-files/roam/20231227192222-dashboard.org"))

(defun append-region-to-scratch (start end)
  "Append the selected region to the *scratch* buffer."
  (interactive "r")  ;; This makes the function use the current region
  (let ((selected-text (buffer-substring start end)))  ;; Extracts the region's text
    (with-current-buffer "*scratch*"  ;; Switches context to the *scratch* buffer
      (goto-char (point-max))  ;; Moves the cursor to the end of the *scratch* buffer
      (insert "\n" selected-text))))  ;; Inserts the selected text at the end

(defun open-predefined-files-in-dired ()
  "Open a Dired buffer with predefined files selected."
  (interactive)
  (dired (cons default-directory '("file1.txt" "file2.txt"))))

(defun insert-custom-current-date-time ()
  "Insert the current date and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun append-output-of-interactive-command-to-scratch (command)
  "Run an interactive COMMAND and append its output to the *scratch* buffer."
  (interactive (list (read-command "Command to execute: ")))
  (let ((output-buffer (generate-new-buffer "*Command Output*")))
    ;; Run the command and capture its output
    (with-output-to-temp-buffer output-buffer
      (save-window-excursion
        (call-interactively command)))
    ;; Append the captured output to the *scratch* buffer
    (with-current-buffer output-buffer
      (let ((output (buffer-string)))
        (with-current-buffer (get-buffer-create "*scratch*")
          (goto-char (point-max))
          (insert "\n" output)
          (display-buffer (current-buffer)))))
    (kill-buffer output-buffer)
    (message "Output appended to *scratch* buffer")))
