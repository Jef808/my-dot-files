;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Local packages
;; (package! tb-dev
;;   :recipe (:local-repo "~/projects/elisp/tb-dev"))

(package! google-c-style
 :recipe (:host github :repo "google/styleguide"
          :files ("google-c-style.el")))

;(package! vue-mode)
;(package! vue-html-mode)
;(package! prettier-js)
(package! add-node-modules-path)

(package! dap-mode :disable t)

(package! systemd)

(package! sly-quicklisp)

(package! lsp-tailwindcss)

(package! lsp-pyright)

(package! know-your-http-well
  :recipe (:host github :repo "for-GET/know-your-http-well"
                 :files ("emacs/*.el")))

;(package! ob-napkin)
;(package! ob-http)
;(package! ob-restclient)
(package! verb)

(package! ox-json)

(package! ox-gfm)

(package! emacs-emojify)

;(package! conda)
(package! nvm)

(package! info-colors)

;(package! auto-yasnippet)

;(package! log4e)

;(package! doxymacs)

;; Until doom supports Emacs 29
(package! transient
      :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
      :recipe (:host github :repo "magit/transient"))
(package! pinentry)
(package! pass)

;; (package! with-editor
;;           :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
;;           :recipe (:host github :repo "magit/with-editor"))

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files  ("*.el" "dist")))

(package! emacs-gif-screencast
  :recipe (:host github :repo "Ambrevar/emacs-gif-screencast"))


(package! graphviz-dot-mode)

;; (package! ellm
;;   :recipe (:host github :repo "Jef808/emacs-llm" :files ("ellm/ellm.el")))

;; ActivityWatch collector
;(package! activity-watch-mode)

;; For writing texts with Jira markup syntax
;(package! jira-markup-mode)




;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
