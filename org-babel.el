;;; org-babel.el -*- lexical-binding: t; -*-
(after! org
  ;; Org Babel configurations for various languages
  (require 'org-tempo)
  (require 'ob-jupyter)
  (use-package! jupyter
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
       (rust . t))
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
    ;; (add-to-list 'org-structure-template-alist '("led" . "src ledger"))
    )

  (after! jupyter
    (setq jupyter-repl-connection-url "http://localhost:8888/tree?token=a15311400477c2a4b7f7e7db661ee8f704968bf2b67dc5a1")))


;; (after! org
;;   ;; Load languages for Org Babel
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (awk . t)
;;      (latex . t)
;;      (shell . t)
;;      ;; (ledger . t) ;; Uncomment if you use ledger
;;      (python . t)))

;;   ;; Disable confirmation prompt for code execution
;;   (setq org-confirm-babel-evaluate nil)

;;   ;; Add additional structure templates
;;   (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
;;   (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;;   (add-to-list 'org-structure-template-alist '("py" . "src python"))
;;   (add-to-list 'org-structure-template-alist '("awk" . "src awk"))
;;   (add-to-list 'org-structure-template-alist '("zig" . "src zig"))
;;   ;; (add-to-list 'org-structure-template-alist '("led" . "src ledger")) ;; Uncomment if you use ledger
;;   )


;; (use-package! org
;;   :config
;;   ;; Load languages for Org Babel
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (awk . t)
;;      (latex . t)
;;      (shell . t)
;;      (python . t)
;;      ;; Add other languages here
;;      ))

;;   ;; Set up Zig template (replace <zig> with <s and then TAB in an Org file)
;;   (add-to-list 'org-structure-template-alist '("zig" . "src zig"))

;;   ;; Disable confirmation prompt for code execution
;;   (setq org-confirm-babel-evaluate nil))
