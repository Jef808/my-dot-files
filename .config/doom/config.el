;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


(setq! user-full-name "Jean-Francois Arbour"
       user-mail-address "jf.arbour@gmail.com")

;; Path variables
;; (setq! user-formatting-dir (file-name-as-directory (expand-file-name "editor/formatting" doom-user-dir))
;;        user-file-templates-dir (file-name-as-directory (expand-file-name "editor/file_templates" doom-user-dir))
;;        user-snippets-dir (file-name-as-directory (expand-file-name "snippets" doom-user-dir)))
(setq Info-additional-directory-list '(("~/.local/share/info")))
;; (add-load-path! user-formatting-dir user-file-templates-dir user-snippets-dir)


(setq global-font-lock-mode t)

(setq display-line-numbers-type t)
(setq scroll-error-top-bottom t
      next-screen-context-lines 4)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(setq envrc-direnv-executable "/usr/bin/direnv")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using the Unix password store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auth-sources '(password-store))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method to generate uuids
(defun +insert-random-uuid ()
  "Insert a UUID.
This command calls 'uuidgen' (on Linux). Taken from
http://xahlee.info/emacs/emacs/elisp_generate_uuid.html"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (shell-command "uuidgen" t))
   (t
    (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                              (user-uid)
                              (emacs-pid)
                              (system-name)
                              (user-full-name)
                              (current-time)
                              (emacs-uptime)
                              (garbage-collect)
                              (buffer-string)
                              (random)
                              (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
                      (substring myStr 0 8)
                      (substring myStr 8 12)
                      (substring myStr 13 16)
                      (format "%x" (+ 8 (random 4)))
                      (substring myStr 17 20)
                      (substring myStr 20 32)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use after-call to load package before hook
(use-package! projectile
  ;; :after-call (pre-command-hook after-find-file dired-before-readin-hook)
  ;; :config
  ;; (defun projectile-project-find-function (dir)
  ;;   (let* ((root (projectile-project-root dir)))
  ;;     (and (const 'transient root)))))
  :commands (projectile-register-project-type)
  :init
  (projectile-register-project-type 'npm '("package.json")
                                           :project-file "package.json"
                                           :compile "npm install"
                                           :test "npm test"
                                           :run "npm start"
                                           :test-suffix ".spec")
  (projectile-register-project-type 'vue '("vite.config.ts")
                                           :project-file "package.json"
                                           :compile "npm run build"
                                           :test "node_modules/vue-tsc/bin/vue-tsc.js --noEmit"
                                           :run "npm run dev"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vertico
  :init
  (vertico-mode))
(use-package savehist
  :init
  (savehist-mode))
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

; (keymap-set vertico-map "?" #'minibuffer-completion-help)
; (keymap-set vertico-map "RET" #'minibuffer-force-complete-and-exit)
; (keymap-set vertico-map "TAB" #'minibuffer-complete)
; (keymap-set vertico-map "M-o" #'consult-multi-occur)


;; Give this a try?
;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-categry-overrides '((file (styles partial-completion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Consult instead of isearch
(map! "C-s" #'consult-line)
(map! "C-r" #'consult-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq company-transformers nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prog mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! compilation-auto-jump-to-first-error t
       compilation-auto-jump-to-next t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sly config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'sly
  `(define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! org-directory (file-name-as-directory "~/org"))
(after! 'org-mode
  (add-to-list 'org-babel-load-languages
               '((http . t)
                 (restclient . t)
                 (python . t)
                 (C++ . t))))

;; diary config
(setq! diary-file (expand-file-name "diary" org-directory)
       diary-comment-start "###")
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

;; org-agenda
(setq! org-agenda-include-diary t)


;; (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
;; (setq org-roam-dailies-directory "daily/")
;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          "* %?"
;;          :target (file+head "%<%Y-%m-%d>.org"
;;                             "#+title: %<%Y-%m-%d>\n"))))
;(org-roam-db-autosync-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! python-interpreter "~/mambaforge/bin/python"
       lsp-ansible-python-interpreter-path "~/mambaforge/bin/python"
       lsp-python-ms-extra-paths '(("~/mambaforge/bin/python"))
       python-shell-exec-path "~/mambaforge/bin/"
       flycheck-python-flake8-executable "~/mambaforge/bin/flake8"
       lsp-clients-pylsp-library-directories "~/mambaforge/"
       pyvenv-default-virtual-env-name "~/mambaforge/env")
(setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
(pyvenv-mode t)
(set-lookup-handlers! 'python-mode
  :definition  #'anaconda-mode-find-definitions
  :references #'anaconda-mode-find-references
  :documentation #'anaconda-mode-show-doc)
(use-package! conda
  :config
  (conda-env-activate "base"))

;; Verb
(use-package! verb
  :after org
  :config
  (defvar verb-edn-request-enabled t)
  (defvar verb-edn-response-enabled t)
  ;; (setq verb-inhibit-cookies t
  ;;       verb-json-use-node 'json-mode)
  (map! :map org-mode-map
        (:localleader "v" verb-command-map
                      (:prefix ("v" . :verb)
                               "r"
#'verb-send-request-on-point-other-window-stay)))

  (map! :map verb-response-body-mode-map
        :n "q" #'kill-buffer-and-window)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((verb . t)
     (typescript . t)))

  (advice-add 'verb--request-spec-post-process :around #'verb--request-spec-post-process-a)

  (add-hook! 'verb-post-response-hook #'verb-post-response-h))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++ config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=2"
          "--malloc-trim"
          "--log=error"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--pch-storage=memory"
          "--header-insertion=never"
          "--header-insertion-decorators=0")))

(add-hook! c-mode-common #'google-set-c-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-lookup-handlers! 'js2-mode :xref-backend #'xref-js2-xref-backend)
(use-package! nvm
  :after +Javascript-Npm)

(use-package! web-mode
  :mode "\\.vue\\'"
  :hook (web-mode . prettier-js-mode)
  :config
  (setq! web-mode-markup-indent-offset 4
         web-mode-code-indent-offset 2
         prettier-js-args '("--parser vue"
                            "--trailing-comma" "all"
                            "--bracket-spacing" "false"))
  (add-hook 'web-mode-hook #'lsp))

(use-package! flycheck
  :hook (flycheck . add-node-modules-path)
  :config
  (setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers '(javascript-jshint json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(defun vue-mode-init-hook ()
  (set-face-background 'mmm-default-submode-face nil))

;; (use-package! vue-mode
;;   :hook (vue-mode . prettier-js-mode)
;;   :mode "\\.vue\\'"
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp)
;;   (add-hook 'vue-mode-hook 'vue-mode-init-hook))

;; (use-package! prettier-js
;;   :config
;;   (add-hook! 'web-mode-hook #'prettier-js-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq! flycheck-cppcheck-standards "--std=c++20"
;;        flycheck-c/c++-cppcheck-executable "/usr/bin/cppcheck"
(setq! flycheck-clang-language-standard "gnu++20")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit / Forge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package forge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! doom-font (font-spec :family "Fira Code" :size 14 :weight 'light)
       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14 :weight 'light)
       doom-theme 'doom-one)

;; Files (from https://github.com/elken/doom#marginalia)
(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log (+ 1 size)) 7.0 10.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info pages (from https://github.com/elken/doom#info-pages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline (from https://github.com/elken/doom#modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! doom-modeline
  (setq all-the-icons-scale-factor 1.1
        auto-revert-check-vc-info t
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p)
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-github t
        doom-modeline-github-interval 60
        doom-modeline-vcs-max-length 60)
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (doom-modeline-def-modeline 'main
    '(bar modals workspace-name window-number persp-name buffer-position selection-info buffer-info matches remote-host debug vcs matches)
    '(github mu4e grip gnus checker misc-info repl lsp " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck (Word spelling checker)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! flyspell-mode
  (set-flyspell-predicate! '(markdown-mode gfm-mode)
                           #'+markdown-flyspell-word-p)

  (defun +markdown-flyspell-word-p ()
    "Return t if point is on a word that should be spell checked.

Return nil if on a link url, markup, html, or references."
    (let ((faces (ensure-list (get-text-property (point) 'face))))
      (or (and (memq 'font-lock-comment-face faces)
               (memq 'markdown-code-face faces))
          (not (cl-loop with unsafe-faces = '(markdown-reference-face
                                              markdown-url-face
                                              markdown-markup-face
                                              markdown-comment-face
                                              markdown-html-attr-name-face
                                              markdown-html-attr-value-face
                                              markdown-html-tag-name-face
                                              markdown-code-face)
                        for face in faces
                        if (memq face unsafe-faces)
                        return t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docstring generator using abo-abo's auto-yas-snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable `docstr' inside these major modes.
(setq! yas-global-mode 1)

(defun moo-doxygen ()
  "Generate a doxygen yasnippet and expand it with `aya-expand'.
The point should be on the top-level function name."
  (interactive)
  (move-beginning-of-line nil)
  (let ((tag (semantic-current-tag)))
    (unless (semantic-tag-of-class-p tag 'fun () ction)
      (error "Expected function, got %S" tag))
    (let* ((name (semantic-tag-name tag))
           (attrs (semantic-tag-attributes tag))
           (args (plist-get attrs :arguments))
           (ord 1))
      (setq aya-current
            (format
             "/**
* $1
*
%s
* @return $%d
*/
"
             (mapconcat
              (lambda (x)
                (format "* @param %s $%d"
                        (car x) (incf ord)))
              args
              "\n")
             (incf ord)))
      (aya-expand))))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
