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

(defun my/org-mode-golden-ratio-hook ()
  (golden-ratio-mode -1))
(add-hook 'org-mode-hook #'my/org-mode-golden-ratio-hook)

(defun my/after-org-mode-golden-ratio-hook ()
  (golden-ratio-mode 1))
(add-hook 'org-mode-hook #'my/after-org-mode-golden-ratio-hook)
