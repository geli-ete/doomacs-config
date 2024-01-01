;;; lsp.el -*- lexical-binding: t; -*-

;; LSP Settings
;; Volar setup for vue 3 projects
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("vls" "--stdio"))
    :major-modes '(vue-mode)
    :server-id 'vls)))
