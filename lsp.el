;; ;;; lsp.el -*- lexical-binding: t; -*-

;; ;; LSP Settings
;; (after! lsp-mode
;;   ;; Golang setup with gopls
;;   (setq lsp-go-gopls-server-path "gopls")
;;   (add-hook 'go-mode-hook #'lsp-deferred)

;;   ;; React setup with tsserver for JavaScript and TypeScript
;;   (setq lsp-clients-typescript-server "typescript-language-server")
;;   (add-hook 'typescript-mode-hook #'lsp-deferred)
;;   (add-hook 'js-mode-hook #'lsp-deferred)
;;   (add-hook 'js2-mode-hook #'lsp-deferred)
;;   (add-hook 'rjsx-mode-hook #'lsp-deferred)
;;   (add-hook 'typescript-tsx-mode-hook #'lsp-deferred)

;;   ;; CSS setup with css-languageserver
;;   (setq lsp-css-server "css-languageserver")
;;   (add-hook 'css-mode-hook #'lsp-deferred)
;;   (add-hook 'scss-mode-hook #'lsp-deferred)
;;   (add-hook 'less-mode-hook #'lsp-deferred)

;;   ;; Vue setup (Volar for Vue 3 projects)
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection '("vls" "--stdio"))
;;     :major-modes '(vue-mode)
;;     :server-id 'vls)))

;; ;; Optional: Set up preferences for LSP behavior
;; (setq lsp-prefer-flymake nil           ;; Use lsp-ui and flycheck instead of flymake
;;       lsp-headerline-breadcrumb-enable t ;; Show breadcrumb in headerline
;;       lsp-enable-symbol-highlighting t   ;; Enable symbol highlighting
;;       lsp-signature-auto-activate nil)   ;; Disable auto-popup of signature help

;; (setq lsp-idle-delay 0.5                 ;; Delay LSP response to reduce load
;;       lsp-completion-provider :capf      ;; Use Emacs' native completion
;;       lsp-enable-file-watchers nil       ;; Disable file watchers for improved performance
;;       lsp-log-io nil)                    ;; Disable logging for better speed

;; (setq company-idle-delay 0.5) ;; Delay in seconds before popup
;; (setq company-minimum-prefix-length 3) ;; Minimum characters before showing suggestions
;; (setq company-idle-delay nil) ;; Disable automatic popup
;; (global-set-key (kbd "TAB") 'company-complete) ;; Or choose another key if preferred
;; (setq company-lsp-cache-candidates 'auto)   ;; Cache completion results
;; (setq company-lsp-async t)                  ;; Enable asynchronous requests
;; (setq company-lsp-enable-snippet nil)       ;; Disable snippet suggestions



;; (setq gc-cons-threshold (* 100 1024 1024)) ;; Increase threshold for fewer interruptions
;;

;;; lsp.el -*- lexical-binding: t; -*-

;; Load and configure lsp-mode
(after! lsp-mode
  ;; --- Golang setup with gopls ---
  (setq lsp-go-gopls-server-path "gopls")
  (add-hook 'go-mode-hook #'lsp-deferred)

  ;; --- JavaScript, TypeScript, and React setup ---
  ;; Typescript and JavaScript configuration with typescript-language-server
  (setq lsp-clients-typescript-server "typescript-language-server")
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (add-hook 'js-mode-hook #'lsp-deferred)
  (add-hook 'js2-mode-hook #'lsp-deferred)
  (add-hook 'rjsx-mode-hook #'lsp-deferred)        ;; React JSX support
  (add-hook 'typescript-tsx-mode-hook #'lsp-deferred) ;; React TSX support

  ;; --- JSON Language Server ---
  ;; Configure JSON language server and associate schemas for specific files
  (setq lsp-json-schemas
        '((:fileMatch ["package.json"] :url "https://json.schemastore.org/package.json")
          (:fileMatch ["tsconfig.json"] :url "https://json.schemastore.org/tsconfig.json")
          (:fileMatch ["*.schema.json"] :url "https://json-schema.org/draft-07/schema")))
  (add-hook 'json-mode-hook #'lsp-deferred)

  ;; --- CSS Language Server (css-languageserver) ---
  ;; Set up CSS/SCSS/Less with LSP support
  (setq lsp-css-server "css-languageserver")
  (add-hook 'css-mode-hook #'lsp-deferred)
  (add-hook 'scss-mode-hook #'lsp-deferred)
  (add-hook 'less-mode-hook #'lsp-deferred)

  ;; --- Vue (Volar) setup for Vue 3 ---
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("vls" "--stdio"))
    :major-modes '(vue-mode)
    :server-id 'vls)))

;; --- General LSP Preferences ---
(setq lsp-prefer-flymake nil                      ;; Use lsp-ui and flycheck instead of flymake
      lsp-headerline-breadcrumb-enable t          ;; Enable breadcrumb in headerline
      lsp-enable-symbol-highlighting t            ;; Highlight symbols under cursor
      lsp-signature-auto-activate nil             ;; Disable automatic signature help popup
      lsp-idle-delay 0.5                          ;; LSP response delay for performance
      lsp-completion-provider :capf               ;; Use Emacs' completion provider
      lsp-enable-file-watchers nil                ;; Disable file watchers for better performance
      lsp-log-io nil)                             ;; Disable verbose logging

;; --- Company Mode Configuration ---
;; Configure company-mode for better performance and usability
(setq company-idle-delay 0.5                      ;; Delay before company popup
      company-minimum-prefix-length 3             ;; Minimum characters before popup
      company-tooltip-align-annotations t         ;; Align annotations to the right
      company-lsp-cache-candidates 'auto          ;; Cache company candidates
      company-lsp-async t                         ;; Enable async requests for company
      company-lsp-enable-snippet nil)             ;; Disable snippet suggestions

;; Set TAB to trigger company completion
(global-set-key (kbd "TAB") 'company-complete)

;; --- Garbage Collection Optimization ---
;; Increase GC threshold for performance, especially with LSP
(setq gc-cons-threshold (* 100 1024 1024))        ;; Set GC threshold for fewer interruptions
