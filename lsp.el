;;; lsp.el -*- lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;; Environment
;; -----------------------------------------------------------------------------
;; Bigger Node heap for TS servers
(setenv "NODE_OPTIONS" "--max-old-space-size=4096")

;; Make Emacs inherit your shell env (GUI launch, nvm/asdf, Go bins, etc.)
(use-package! exec-path-from-shell
  :init
  (exec-path-from-shell-copy-envs '("PATH" "NODE_PATH" "NVM_DIR" "GOPATH" "GOBIN")))

;; -----------------------------------------------------------------------------
;; Core LSP (shared)
;; -----------------------------------------------------------------------------
;; Keep plists (faster); set to t if you need alists for some reason.
(setq lsp-use-plists nil)

(after! lsp-mode
  (setq lsp-enable-file-watchers nil
        lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-completion-provider :capf
        lsp-semantic-tokens-enable nil
        lsp-enable-on-type-formatting nil
        lsp-enable-formatting t            ;; allow server formatting (we disable per-buffer as needed)
        lsp-signature-auto-activate nil
        lsp-eldoc-hook nil)
  ;; ignore common heavy dirs for file-watching
  (dolist (dir '("[/\\\\]node_modules\\'"
                 "[/\\\\]dist\\'"
                 "[/\\\\]build\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]vendor\\'"))
    (cl-pushnew dir lsp-file-watch-ignored-directories :test #'equal)))

;; Start LSP for common modes
(dolist (hk '(go-mode-hook go-ts-mode-hook
              json-mode-hook json-ts-mode-hook
              css-mode-hook scss-mode-hook less-css-mode-hook css-ts-mode-hook
              toml-mode-hook
              typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook js2-mode-hook rjsx-mode-hook))
  (add-hook hk #'lsp-deferred))

;; -----------------------------------------------------------------------------
;; JSON Schemas (safe: DO NOT globally override Emacs JSON types)
;; -----------------------------------------------------------------------------
(after! lsp-mode
  (setq lsp-json-schemas
        '((:fileMatch ["package.json"]  :url "https://json.schemastore.org/package.json")
          (:fileMatch ["tsconfig.json"] :url "https://json.schemastore.org/tsconfig.json")
          (:fileMatch ["*.schema.json"] :url "https://json-schema.org/draft-07/schema"))))

;; IMPORTANT: Do NOT set json-object-type/json-array-type globally.
;; (That caused: let: Wrong type argument: listp, #s(hash-table ...))

;; -----------------------------------------------------------------------------
;; TypeScript / TSX / JS — ts-ls + Prettier via Apheleia
;; -----------------------------------------------------------------------------
(with-eval-after-load 'lsp-mode
  ;; Use the official tsserver (tsserver/js-language-features a.k.a ts-ls)
  (setq lsp-clients-typescript-server "ts-ls"
        ;; Disable other JS/TS clients we won't use
        lsp-disabled-clients
        '(deno-ls jsts-ls emmet-ls graphql-lsp eslint
          deno-ls-tramp jsts-ls-tramp emmet-ls-tramp graphql-lsp-tramp eslint-tramp)))

(defun +my/lsp-setup-ts-js ()
  (setq-local lsp-enabled-clients '(ts-ls))
  ;; Local buffers → Prettier via Apheleia; remote (TRAMP) → LSP formatting
  (setq-local +format-with-lsp (file-remote-p default-directory))
  (lsp-deferred))

(dolist (m '(typescript-ts-mode tsx-ts-mode js-ts-mode js2-mode rjsx-mode js-mode))
  (add-hook m #'+my/lsp-setup-ts-js))

;; If a project has a local TS, point lsp to it (optional)
(with-eval-after-load 'lsp-mode
  (let ((tss (ignore-errors
               (expand-file-name "node_modules/typescript/lib/tsserver.js"
                                 (lsp-workspace-root default-directory)))))
    (when (and tss (file-exists-p tss))
      (setq lsp-clients-typescript-server-args
            (list "--tsserver-path" tss)))))

;; -----------------------------------------------------------------------------
;; Apheleia — Prettier for TS/JS and CSS-family
;; -----------------------------------------------------------------------------
(setq +format-with-lsp nil)  ;; default off; we enable per-buffer where desired

(after! apheleia
  ;; Remove any stale lsp-format entry
  (setq apheleia-formatters (assq-delete-all 'lsp-format apheleia-formatters))

  ;; Prefer project-local Prettier via npx --yes
  (setf (alist-get 'prettier apheleia-formatters)
        '("npx" "--yes" "prettier" "--stdin-filepath" filepath))

  ;; TS/JS → Prettier
  (dolist (pair '((tsx-ts-mode        . prettier)
                  (typescript-ts-mode . prettier)
                  (js-ts-mode         . prettier)
                  (js2-mode           . prettier)
                  (rjsx-mode          . prettier)))
    (setf (alist-get (car pair) apheleia-mode-alist) (cdr pair)))

  ;; CSS family → Prettier (keeps formatting consistent across front-end)
  (dolist (pair '((css-mode       . prettier)
                  (css-ts-mode    . prettier)
                  (scss-mode      . prettier)
                  (less-css-mode  . prettier)))
    (setf (alist-get (car pair) apheleia-mode-alist) (cdr pair)))

  ;; Never let Apheleia touch Go (we use goimports directly)
  (dolist (m '(go-mode go-ts-mode))
    (setf (alist-get m apheleia-mode-alist) nil))

  (setq apheleia-log-command-output t))

;; Disable LSP formatting in CSS-family (let Apheleia/Prettier handle it)
(dolist (hk '(css-mode-hook scss-mode-hook less-css-mode-hook css-ts-mode-hook))
  (add-hook hk (lambda () (setq-local +format-with-lsp nil))))

;; -----------------------------------------------------------------------------
;; Go — force goimports on save (native gofmt-before-save), no LSP/Apheleia fmt
;; -----------------------------------------------------------------------------
(setq gofmt-command "goimports")

(defun geli--go-buffer-setup ()
  "Disable competing formatters and ensure gofmt-before-save for Go buffers."
  (setq-local +format-with-lsp nil)
  (when (boundp 'apheleia-mode) (setq-local apheleia-mode nil))
  (remove-hook 'before-save-hook #'+format-buffer-h t)
  (add-hook 'before-save-hook #'gofmt-before-save nil t)
  (when (boundp 'ws-butler-mode) (ws-butler-mode -1))
  (message "[GO-FMT] gofmt-before-save attached; ws-butler disabled; major=%s" major-mode))

(add-hook 'go-mode-hook    #'geli--go-buffer-setup)
(add-hook 'go-ts-mode-hook #'geli--go-buffer-setup)

;; Keep LSP features, but not LSP formatting
(add-hook 'go-mode-hook    (lambda () (setq-local +format-with-lsp nil) (lsp-deferred)))
(add-hook 'go-ts-mode-hook (lambda () (setq-local +format-with-lsp nil) (lsp-deferred)))

(after! lsp-mode
  (setq lsp-go-gopls-server-path "gopls"))

;; Also run a last-chance formatter before save (belt & suspenders)
(defun geli--go-format-before-save-advice (&rest _)
  (when (derived-mode-p 'go-mode 'go-ts-mode)
    (setq-local gofmt-command "goimports")
    (gofmt)
    (message "[GO-FMT] Ran gofmt via advice before save.")))
(advice-add 'save-buffer :before #'geli--go-format-before-save-advice)

;; -----------------------------------------------------------------------------
;; Rust & TOML
;; -----------------------------------------------------------------------------
(after! lsp-mode
  (setq lsp-rust-analyzer-cargo-watch-command "check"
        lsp-rust-analyzer-cargo-all-features t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-completion-add-call-parenthesis t
        lsp-rust-analyzer-diagnostics-enable t))

(after! rustic
  (setq rustic-format-on-save t))

(use-package! lsp-toml :after lsp-mode)

;; -----------------------------------------------------------------------------
;; Tree-sitter
;; -----------------------------------------------------------------------------
(setq treesit-language-source-alist
      '((tsx        "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")))

(use-package! treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(dolist (lang '(typescript))
  (unless (treesit-language-available-p lang)
    (ignore-errors (treesit-install-language-grammar lang))))

;; -----------------------------------------------------------------------------
;; Final notes:
;; - If you previously had this anywhere, remove it:
;;     (setq json-object-type 'hash-table
;;           json-array-type  'vector
;;           json-false       :json-false)
;;   It breaks packages that expect alists and caused your error while saving .css.
;; -----------------------------------------------------------------------------

(provide 'lsp)
;;; lsp.el ends here
