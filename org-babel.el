;;; org-babel.el -*- lexical-binding: t; -*-

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


(use-package! org
  :config
  ;; Load languages for Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (awk . t)
     (latex . t)
     (shell . t)
     (python . t)
     ;; Add other languages here
     ))

  ;; Set up Zig template (replace <zig> with <s and then TAB in an Org file)
  (add-to-list 'org-structure-template-alist '("zig" . "src zig"))

  ;; Disable confirmation prompt for code execution
  (setq org-confirm-babel-evaluate nil))
