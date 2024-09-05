;;; org-babel.el -*- lexical-binding: t; -*-
(after! org
  ;; Org Babel configurations for various languages
  (require 'org-tempo)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((zig . t)
     (lisp . t)
     (c . t)
     (cpp . t)
     (awk . t)
     (latex . t)
     (sed . t)
     (jupyter . t)
     (emacs-lisp . t)
     (shell . t)
     ;; (rust . t))
     (add-to-list 'load-path "~/.doom.d/external/")
     (require 'ob-zig))
   (setq org-babel-jupyter-resource-directory "~/.doom.d/.local/cache/jupyter")

   ;; Add additional structure templates
   (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
   (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
   (add-to-list 'org-structure-template-alist '("py" . "src python"))
   (add-to-list 'org-structure-template-alist '("awk" . "src awk"))
   (add-to-list 'org-structure-template-alist '("zig" . "src zig"))
   (add-to-list 'org-structure-template-alist '("li" . "src lisp"))

   ;; Uncomment the next line if you use ledger
   (add-to-list 'org-structure-template-alist '("led" . "src ledger"))
   ))

;;<2024-01-12 Fri 18:07>
;; Create a custom tangle function
(defun my/org-babel-tangle-config ()
  (when (string-equal (file-name-extension buffer-file-name) "org")
    (org-babel-tangle)))

;; add the function to save hook.
;; for 'before-save-hook'
(add-hook 'before-save-hook #'my/org-babel-tangle-config)
;; or for 'after-save-hook'
;; (add-hook 'after-save-hook #'my/org-babel-tangle-config)
