;;; lsp.el -*- lexical-binding: t; -*-

;; ------------------------------------------------------------
;; Core LSP (shared)
;; ------------------------------------------------------------
;; Use hash tables everywhere with LSP/JSON
(setq lsp-use-plists nil)

(after! lsp-mode
  ;; Optional perf optimization (less consing)

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

  ;; Add common noisy dirs to file-watch ignore list (idempotent)
  (dolist (dir '("[/\\\\]node_modules\\'"
                 "[/\\\\]dist\\'"
                 "[/\\\\]build\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]vendor\\'"))
    (cl-pushnew dir lsp-file-watch-ignored-directories :test #'equal)))

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
;; Use a single client (ts-ls) for both local & TRAMP.
;; Keep ESLint for diagnostics; Prettier handles local formatting (Apheleia).
;; ------------------------------------------------------------
(with-eval-after-load 'lsp-mode
  ;; Force typescript-language-server; avoid deno/vtsls/etc unless you opt in.
  (setq lsp-clients-typescript-server "ts-ls"
        lsp-disabled-clients
        '(deno-ls jsts-ls emmet-ls graphql-lsp eslint
          deno-ls-tramp jsts-ls-tramp emmet-ls-tramp graphql-lsp-tramp eslint-tramp)))

(defun +my/lsp-setup-ts-js ()
  ;; One client works everywhere (local & remote)
  (setq-local lsp-enabled-clients '(ts-ls))
  ;; Remote (TRAMP) -> format with LSP; Local -> Prettier via Apheleia
  (setq-local +format-with-lsp (file-remote-p default-directory))
  (lsp-deferred))

(dolist (m '(typescript-ts-mode tsx-ts-mode js-ts-mode js2-mode rjsx-mode js-mode))
  (add-hook m #'+my/lsp-setup-ts-js))

;; ESLint (diagnostics only)
(with-eval-after-load 'lsp-mode
  (require 'lsp-eslint)
  (setq lsp-eslint-format nil
        ;; Adjust for monorepos if needed:
        ;; lsp-eslint-working-directories [ "./" "packages/*" "apps/*" ]
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
  ;; Don’t auto-format in merge/ediff/smerge/magit/git-commit buffers
  (dolist (h '(ediff-prepare-buffer-hook smerge-mode-hook magit-status-mode-hook git-commit-setup-hook))
    (add-hook h (lambda () (when (bound-and-true-p apheleia-mode) (apheleia-mode -1)))))
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

;; ~/.doom.d/config.el
(setq treesit-language-source-alist
      '((tsx        "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")))

;; ~/.doom.d/config.el
(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)  ;; or 't to install silently
  :config
  (global-treesit-auto-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(dolist (lang '(typescript))
  (unless (treesit-language-available-p lang)
    (ignore-errors (treesit-install-language-grammar lang))))

;; Use native JSON defaults that lsp-mode expects
(setq json-object-type 'hash-table
      json-array-type 'vector
      json-false :json-false)

(with-eval-after-load 'lsp-mode
  ;; Point typescript-language-server at the workspace’s tsserver if available.
  ;; Adjust the path if your node_modules is not at project root.
  (let ((tss (expand-file-name "node_modules/typescript/lib/tsserver.js"
                               (lsp-workspace-root default-directory))))
    (when (file-exists-p tss)
      (setq lsp-clients-typescript-server-args
            (list "--tsserver-path" tss)))))
