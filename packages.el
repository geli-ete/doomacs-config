;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! exec-path-from-shell)
(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"))

(package! lsp-tailwindcss) ; Tailwind CSS LSP (optional)
(package! prettier)       ; Prettier minor-mode (optional)

(package! org-roam-ui)
;; org-roam-ui requires websocket at runtime; pinning not necessary typically
(package! websocket)

;;(package! geli-mode
;;  :recipe (:local-repo "~/geli-mode"
;;           :files ("*.el")))

(package! org-superstar)
;; (package! vertico)
;; (package! posframe)
(package! vertico-posframe)
(package! centaur-tabs)
(package! all-the-icons-dired)
(package! sly)
(package! pdf-tools)
(package! jupyter)

(package! nov)
(package! djvu)

(package! undo-fu)

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; `themes'
(package! spacemacs-theme)
(package! gruber-darker-theme)
(package! color-theme-sanityinc-tomorrow)  ;; Package for Tomorrow themes
(package! material-theme)

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)
