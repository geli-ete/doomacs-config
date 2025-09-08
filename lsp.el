;;; lsp.el -*- lexical-binding: t; -*-

;; ------------------------------------------------------------
;; Core LSP (shared)
;; ------------------------------------------------------------
(after! lsp-mode
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil
        lsp-headerline-breadcrumb-enable t
        lsp-enable-symbol-highlighting t
        lsp-signature-auto-activate nil
        lsp-idle-delay 0.35
        lsp-completion-provider :capf
        lsp-enable-file-watchers t
        lsp-log-io nil
        lsp-semantic-tokens-enable t)
  (dolist (dir '("[/\\\\]node_modules\\'"
                 "[/\\\\]dist\\'"
                 "[/\\\\]build\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]vendor\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir)))

;; Start LSP for common modes (TS/JS handled below, custom)
(dolist (hk '(go-mode-hook go-ts-mode-hook
              json-mode-hook json-ts-mode-hook
              css-mode-hook scss-mode-hook less-mode-hook css-ts-mode-hook
              toml-mode-hook))
  (add-hook hk #'lsp-deferred))

;; ------------------------------------------------------------
;; Go (gopls)
;; ------------------------------------------------------------
(after! lsp-mode
  (setq lsp-go-gopls-server-path "gopls"))

(defun +my/lsp-setup-go ()
  ;; Always use gopls for formatting (works local & TRAMP)
  (setq-local +format-with-lsp t)
  (lsp-deferred))

(add-hook 'go-mode-hook     #'+my/lsp-setup-go)
(add-hook 'go-ts-mode-hook  #'+my/lsp-setup-go)

;; ------------------------------------------------------------
;; TypeScript / TSX / JS
;; Use ts-ls locally, ts-ls-tramp over TRAMP.
;; Keep ESLint for diagnostics only; Prettier handles formatting locally.
;; ------------------------------------------------------------
(with-eval-after-load 'lsp-mode
  ;; Make sure we use ts-ls (not vtsls) to match the available TRAMP clients
  (setq lsp-clients-typescript-server "ts-ls"
        ;; Silence extras you don't use
        lsp-disabled-clients
        '(deno-ls jsts-ls emmet-ls graphql-lsp eslint
          deno-ls-tramp jsts-ls-tramp emmet-ls-tramp graphql-lsp-tramp eslint-tramp)))

(defun +my/lsp-setup-ts-js ()
  ;; Pick client based on local/remote buffer
  (setq-local lsp-enabled-clients
              (list (if (file-remote-p default-directory) 'ts-ls-tramp 'ts-ls)))
  ;; Formatting strategy:
  ;; - Local: Prettier via Apheleia (default below)
  ;; - Remote (TRAMP): use LSP formatting
  (setq-local +format-with-lsp (file-remote-p default-directory))
  (lsp-deferred))

(dolist (m '(typescript-ts-mode tsx-ts-mode js-ts-mode js2-mode rjsx-mode js-mode))
  (add-hook m #'+my/lsp-setup-ts-js))

;; ESLint (diagnostics only)
(with-eval-after-load 'lsp-mode
  (require 'lsp-eslint)
  (setq lsp-eslint-format nil
        lsp-eslint-working-directories ["./"]))

;; ------------------------------------------------------------
;; JSON (schemas)
;; ------------------------------------------------------------
(after! lsp-mode
  (setq lsp-json-schemas
        '((:fileMatch ["package.json"]  :url "https://json.schemastore.org/package.json")
          (:fileMatch ["tsconfig.json"] :url "https://json.schemastore.org/tsconfig.json")
          (:fileMatch ["*.schema.json"] :url "https://json-schema.org/draft-07/schema"))))

;; ------------------------------------------------------------
;; Format-on-save wiring (Doom's :editor (format +onsave))
;; TS/TSX/JS: Prettier locally (Apheleia). Remote overridden to LSP above.
;; Go: LSP (gopls).
;; ------------------------------------------------------------
(setq +format-with-lsp nil) ;; default; per-buffer overrides above

(after! apheleia
  ;; Fallback formatter that calls lsp-format-buffer
  (unless (assq 'lsp-format apheleia-formatters)
    (add-to-list 'apheleia-formatters '(lsp-format . (lsp-format-buffer))))
  ;; Prettier (local)
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  ;; Local TS/JS → Prettier; remote is switched to LSP by +format-with-lsp t
  (dolist (pair '((tsx-ts-mode        . prettier)
                  (typescript-ts-mode . prettier)
                  (js-ts-mode         . prettier)
                  (js2-mode           . prettier)
                  (rjsx-mode          . prettier)))
    (setf (alist-get (car pair) apheleia-mode-alist) (cdr pair)))
  ;; Go → LSP
  (dolist (mode '(go-mode go-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'lsp-format)))

;; ------------------------------------------------------------
;; Rust & TOML
;; ------------------------------------------------------------
(after! lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "check"
        lsp-rust-analyzer-cargo-all-features t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-completion-add-call-parenthesis t
        lsp-rust-analyzer-diagnostics-enable t))
(after! rustic
  (setq rustic-format-on-save t))
(use-package! lsp-toml :after lsp-mode)
