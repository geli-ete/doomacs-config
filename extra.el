;;; extra.el -*- lexical-binding: t; -*-
;; Remove default doom banner at startup
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-banner)

;; SLY (Sylvester the Cat's Common Lisp IDE)
(after! sly
  ;; Set the default Lisp interpreter to a Roswell-managed implementation
  (setq inferior-lisp-program "ros -Q run"))

(require 'c3po)
(setq c3po-api-key "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")

;; Extra Configurations
(repeat-mode)
(auto-revert-mode) ;; For auto refresh of changed buffer

;; Org-Emphasis is mostly hidden except when your cursor in on that word.
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

;; adjusts the size of Emacs windows automatically using the golden ratio principle
(use-package! golden-ratio
  :config
  (golden-ratio-mode 1)
  ;;(setq golden-ratio-exclude-modes '("org-mode"
  ;; Add other modes if necessary
  ;;                                  ))
  ;; Optional configurations go here. For example:
  (setq golden-ratio-adjust-factor .8
        golden-ratio-wide-adjust-factor .8)
  )

(use-package! vertico
  :init
  (vertico-mode)
  :config
  (use-package vertico-posframe
    :config
    (vertico-posframe-mode 1)))

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
