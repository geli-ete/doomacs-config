;;; dired.el -*- lexical-binding: t; -*-

(after! dired
  (use-package! all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

  ;; Set Dired to use human-readable file sizes (e.g., KB, MB)
  (setq dired-listing-switches "-alh")

  ;; Always recreate Dired buffers when revisiting a directory.
  ;; This ensures that the directory listing is always up-to-date.
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; Custom keybindings for Dired
  (map! :map dired-mode-map
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file
        :n "H" #'dired-hide-details-mode  ;; Toggle more/less details
        :n "zh" #'dired-omit-mode)        ;; Toggle visibility of hidden files

  ;; Optionally, automatically hide details by default
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  ;; Omit mode settings - hide certain files by default (like dotfiles)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")

  ;; Colorful dired
  (use-package! diredfl
    :config
    (diredfl-global-mode 1))

  ;; Optional: Use icons in Dired (if you like a more graphical interface)
  (use-package! all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))
