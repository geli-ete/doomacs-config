;;; lsp.el -*- lexical-binding: t; -*-

;; LSP Settings

;; Volar setup for Vue 3 projects
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("vls" "--stdio"))
    :major-modes '(vue-mode)
    :server-id 'vls)))

;; Go LSP (gopls) settings
(after! lsp-mode
  (setq lsp-disabled-clients '(semgrep-ls golangci-lint))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "gopls")
    :major-modes '(go-mode)
    :server-id 'gopls)))

;; GolangCI-Lint (Optional Linter)
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "golangci-lint-langserver")
    :major-modes '(go-mode)
    :server-id 'golangci-lint)))

;; Semgrep (Static code analysis)
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "semgrep lsp")
    :major-modes '(go-mode vue-mode js-mode)
    :server-id 'semgrep-ls)))

;; Enable auto-guess for LSP root directories
(after! lsp-mode
  (setq lsp-auto-guess-root t))
