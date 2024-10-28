;;; config.el -*- lexical-binding: t; -*-

(require 'dash)

;;; Code:

(setq! user-full-name "Jean-Francois Arbour"
       user-mail-address "jf.arbour@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq global-font-lock-mode t)
(setq display-line-numbers-type t)
(setq scroll-error-top-bottom t
      next-screen-context-lines 4)

(setq tab-width 4)

;; Disable customization
(setq custom-file nil)

;; Disable the mouse
(mouse-avoidance-mode 'cat-and-mouse)
(setq mouse-wheel-mode nil)
(define-key global-map [mouse-2] 'ignore)
(define-key global-map [mouse-3] 'ignore)
(define-key global-map [down-mouse-2] 'ignore)
(define-key global-map [down-mouse-3] 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom advices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jf/mark-thing1 (mark-func &optional args interactive)
  "Advice which prevents `MARK-FUNC' to overshoot.
\(`ARGS' and `INTERACTIVE' are as in `mark-defun'.
The builtin `mark-defun' function includes commented and/or
empty lines found before the function it is marking. With this,
when doing \\[mark-defun] only the `defun' form is marked."
  (let ((beginning-of-defun-function #'beginning-of-defun))
    (funcall mark-func args interactive)))

(advice-add 'mark-defun :around #'jf/mark-thing1)
;(advice-add 'mark-paragraph :around #'jf/mark-thing1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! doom-font (font-spec :family "JetBrains Mono" :size 12)
       doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14)
       doom-theme 'doom-dracula)

(setq envrc-direnv-executable "/usr/bin/direnv")
(setq inhibit-x-resources nil)

(defun jf/browse-url-brave (url &optional _new-window)
  "Ask the Brave WWW browser to load URL.
Default to the URL around or before point. The strings in
variable `jf/browse-url-brave-arguments' are also passed to
Brave.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
       (concat "brave " url) nil
       jf/browse-url-brave-program
       (append
        jf/browse-url-brave-arguments
        (list url)))))

(setq browse-url-chrome-program "/usr/bin/google-chrome-stable"
      browse-url-browser-function 'jf/browse-url-brave
      jf/browse-url-brave-program "/usr/bin/brave"
      jf/browse-url-brave-arguments nil)


;; Map \\[execute-extended-command] to C-c C-m and C-x C-m
;; since it's faster than M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Map \\[backward-kill-word] to C-w (the default in bash)
(global-set-key (kbd "C-w") 'backward-kill-word)

;; Remap \\[kill-region] to C-x C-k (it was C-w)
(global-set-key (kbd "C-x C-k") 'kill-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Sitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(mapc (lambda (e) (set-tree-sitter-lang! (car e) (cdr e)))
      `((c++-ts-mode . h)
        (c++-ts-mode . cpp)
        (typescript-ts-mode . typescript)
        (tsx-ts-mode . typescript)
        (python-ts-mode . python)
        (css-ts-mode . css)
        (json-ts-mode . json)
        (yaml-ts-mode . yaml)
        (rust-ts-mode . rust)))
(setq treesit-extra-load-path '("~/.local/share/tree-sitter-gdscript/src/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github Copilot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accept completion from copilot and fallback to company
;; (use-package copilot
;;   :defines (copilot-mode copilot-completion-map)
;;   :commands (copilot-accept-completion)
;;   :init
;;   :hook (prog-mode . copilot-mode)
;;   :config
;;   (setq jf/copilot-tab-or-default
;;           #'(lambda ()
;;                     (if (and (bound-and-true-p copilot-mode))
;;                             (copilot-accept-completion))))
;;   (map! :map
;;         copilot-completion-map
;;         "<tab>"   #'jf/copilot-tab-or-default
;;         "TAB"     #'jf/copilot-tab-or-default
;;         "C-TAB"   #'copilot-accept-completion-by-word
;;         "C-<tab>" #'copilot-accept-completion-by-word))


;; (use-package! copilot
;;   :hook 'prog-mode copilot-mode)

(map! :after copilot
      :map copilot-completion-map
        "<tab>"    #'copilot-accept-completion
        "TAB"      #'copilot-accept-completion
        "C-TAB"    #'copilot-accept-completion-by-word
        "C-<tab>"  #'copilot-accept-completion-by-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communicate with LLMs using ellm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load (expand-file-name "~/projects/emacs/ellm/ellm.el"))
;; (require 'ellm)
;; (global-ellm-mode)

(defun jf/api-key-pw-store (provider)
  "Get api key for `PROVIDER' from the password store."
  (let ((jf/pw-store-paths
         '((openai . "openai/ellm_api_key")
           (anthropic . "anthropic/ellm_api_key")
           (groq . "groq/ellm_api_key")
           (mistral . "mistral/ellm_api_key"))))
    (or (encode-coding-string
         (string-trim-right (shell-command-to-string (format "pass %s" (alist-get provider jf/pw-store-paths))))
         'utf-8)
        (error "config.el: jf/api-keys-pw-store: Unknown provider: %s" (symbol-name provider)))))



(use-package! ellm
  :custom
  ellm-api-key #'jf/api-key-pw-store
  :config
  (ellm-setup-persistance)
  (ellm-start-server)
  (global-ellm-mode))

;; (debug-on-entry 'ellm--context-buffer-setup)
;; (debug-on-entry 'ellm--setup-persistance)

;; (defvar ellm-system-message-elisp "You are a useful emacs-integrated general assistant, expert in writing emacs-lisp and \
;; in the technicalities of the emacs packages ecosystem in general.
;; Your goal is to provide emacs-lisp code snippets, explanations, and general guidance to the user, \
;; according to their needs regarding emacs-lisp programming.
;; When the user provides CONTEXT, you should use it to provide more accurate and relevant answers.
;; You are very cautious when providing information or making a claim, thus always accompanying them with \
;; thorough explanations and/or justifications when providing non-obvious answers.
;; Always answer with a request for clarifications when you are not fully confident on how to address a user's query."
;;   "The system message for asking questions about emacs lisp in ellm.")

;; (defun my-extarct-python-function ()
;;   "Extract the Python function definition at the current point."
;;   (save-excursion  ; Preserve the original cursor position
;;     (let (start end function-text)
;;       (python-nav-beginning-of-defun)  ; Go to the start of the function
;;       (setq start (point))  ; Mark the start position
;;       (python-nav-end-of-defun)  ; Go to the end of the function
;;       (setq end (point))  ; Mark the end position
;;       (setq function-text (buffer-substring-no-properties start end))  ; Extract the function text
;;       (message function-text))))  ; Display the extracted text

;; (defun my-insert-python-docstring ()
;;   "Generate and insert a docstring describing the Python function at point."
;;   (let ((docstring (shell-command "gen-python-docstring2" (my-extract-python-function))))
;;     (save-excursion  ; Preserve the original cursor position
;;       (python-nav-beginning-of-defun)  ; Go to the start of the function
;;       (forward-line)  ; Move to the line after the def line
;;       (let ((indentation (make-string (current-indentation) ?\s)))  ; Store the current indentation level
;;         (if (looking-at (concat indentation "\"\"\""))  ; Check if a docstring already exists
;;             (delete-region (point) (progn (forward-sexp) (point)))  ; Remove the existing docstring
;;           (open-line 1))  ; Otherwise, open a line for the new docstring
;;         (insert (concat indentation "\"\"\"" docstring "\"\"\"\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using the Unix password store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq auth-sources '(password-store))
;; (setq epg-pinentry-mode 'loopback)
;; (pinentry-start)

;;;;;;;;;;;;;
;; Buffers ;;
;;;;;;;;;;;;;
;; (defun ibuffer-filter-by-workspace (buf)
;;   "Filter buffers by their workspace."
;;   (let ((ws (workspace-name (buffer-local-value 'workspace-current-name buf))))
;;     (if ws
;;         (string= ws (workspace-current-name))
;;       (not (workspace-buffer-p buf)))))

;; (defun ibuffer-group-by-workspace ()
;;   "Group buffers by their workspace."
;;   (ibuffer-make-column-alist)
;;   (ibuffer-do-sort-by-alphabetic)
;;   (ibuffer-update nil t)
;;   (ibuffer-forward-line 0)
;;   (ibuffer-filter-by-predicate 'ibuffer-filter-by-workspace))

;; (add-hook 'ibuffer-mode-hook
;;           (lambda ()
;;             (ibuffer-switch-to-saved-filter-groups "workspaces")
;;             (unless (eq ibuffer-filtering-qualifiers '((workspace . ibuffer-filter-by-workspace)))
;;               (setq ibuffer-filtering-qualifiers '((workspace . ibuffer-filter-by-workspace)))
;;               (ibuffer-update nil t))))

;; (setq ibuffer-saved-filter-groups
;;       '(("workspaces"
;;          ("emacs" (workspace . "emacs"))
;;          ("Workspace 1" (workspace . "1"))
;;          ("Workspace 2" (workspace . "2"))
;;          ("Workspace 3" (workspace . "3"))
;;          ("Other" (workspace . nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make the `transpose-sexp' function preserve the point position
(defun my-transpose-sexps ()
  "Transpose the sexp at point and preserve the point position."
  (interactive)
  (let ((posn (point)))
    (transpose-sexps 1)
    (goto-char posn)))

;; Method to generate uuids
;; (defun jf/insert-random-uuid ()
;;   "Insert a UUID.
;; This command calls 'uuidgen' (on Linux). Taken from
;; http://xahlee.info/emacs/emacs/elisp_generate_uuid.html"
;;   (interactive)
;;   (cond
;;    ((string-equal system-type "gnu/linux")
;;     (shell-command "uuidgen" t))
;;    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (after! projectile
;;   :commands (projectile-register-project-type)
;;   :init
;;   (projectile-register-project-type 'npm '("package.json")
;;                                            :project-file "package.json"
;;                                            :compile "npm install"
;;                                            :test "npm test"
;;                                            :run "npm start"
;;                                            :test-suffix ".spec")
;;   (projectile-register-project-type 'vue '("vite.config.ts")
;;                                            :project-file "package.json"
;;                                            :compile "npm run build"
;;                                            :test "node_modules/vue-tsc/bin/vue-tsc.js --noEmit"
;;                                            :run "npm run dev"))
(after! projectile (setq projectile-indexing-method 'alien))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for googlings things at point
;; It uses the keybinding prefix `C-c /' by default
(use-package! google-this
  :commands google-this-mode
  :init
  (google-this-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! company
  :defer 0.1
  :config
  (setq-default
   company-idle-delay 0.5
   company-require-match nil
   company-minimum-prefix-length 0))
; (setq company-transformers nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prog mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! compilation-auto-jump-to-first-error t
       compilation-auto-jump-to-next t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sly config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! sly
  :config
  (define-key sly-prefix-map (kbd "M-h") 'sly-documentation-lookup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! org-protocol-capture-html
;;   :config
;;   (setq org-protocol-capture-html-templates
;;         '(("w" "Web site" entry
;;            (file "")
;;            "* %a :website:\n\n%U %?\n\n%:initial"))))

(setq org-directory "~/org/")
(after! org-mode
  ;; (setq org-startup-folded 'show2levels
  (setq org-ellipsis " [...] ")
  (setq org-todo-keyword-faces
        '(("TODO" . +org-todo-onhold)
          ("NEXT" . +org-todo-active)
          ("SOMEDAY" . +org-todo-project)
          ("[-]" . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO" . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (C . t)
     (C++ . t)
     (comint . t)
     (css . t)
     (elisp . t)
     (eshell . t)
     (gnuplot . t)
     (screen . t)
     (sed . t)
     (dot . t)
     (java . t)
     (latex . t)
     (lisp . t)
     (lua . t)
     (makefile . t)
     (js . t)
     (typescript-tsx . t)
     (org . t)
     (python . t)
     (sass . t)
     (sql . t)
     (sqlite . t)
     (restclient . t)))
  (setf (alist-get "C++" org-src-lang-modes nil nil 'equal) 'c++-ts
        (alist-get "bash" org-src-lang-modes nil nil 'equal) 'bash-ts
        (alist-get "cpp" org-src-lang-modes nil nil 'equal) 'c++-ts
        (alist-get "python" org-src-lang-modes nil nil 'equal) 'python-ts))
;; org export
;; https://github.com/fniessen/org-html-themes
;; Use #+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup

;; (setq! org-latex-listings 'minted
;;        org-latex-packages-alist '(("" "minted"))
;;        org-latex-pdf-process
;;        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; diary config
;; (setq! diary-file (expand-file-name "diary" org-directory)
;;        diary-comment-start "###")
;; (add-hook 'diary-list-entries-hook 'diary-sort-entries t)

;; org-agenda
;(setq! org-agenda-include-diary t)
;; (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
;; (setq org-roam-dailies-directory "daily/")
;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          "* %?"
;;          :target (file+head "%<%Y-%m-%d>.org"
;;                             "#+title: %<%Y-%m-%d>\n"))))
;(org-roam-db-autosync-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lsp Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (after! lsp-mode
;;   (setq lsp-enable-symbol-highlighting nil))
;; (after! lsp-ui
;;   (setq lsp-ui-sideline-enable nil
;;         lsp-ui-doc-enable nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq! lsp-python-server-command '("python-language-server" "--stdio")
;;        lsp-python-server-args '("--client-id" "emacs" "--project" "." "--log-level" "info")
;;        lsp-python-pyright-command '("pyright" "langserver" "--stdio"))

;; (add-to-list 'lsp-language-id-configuration '(python-mode . "pyright"))

(after! python-ts-mode
  (setq flycheck-python-pylint-executable "pylint"))
;       flycheck-python-black-executable "black")

;(defun my-make-python-docstring ()
;  (interactive)
;  (let ((line-content (thing-at-point 'line t)))
;    (when (string-match "^\\s-+#\\s-" line-content)
;      (delete-region (line-beginning-position) (line-end-position))
;      (insert (format "\"\"\"%s\"\"\"" (string-trim-left (thing-at-point 'line t) "\\s-+#\\s-+"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++ config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook! 'c++-ts-mode-hook (setq c-buffer-is-cc-mode t))
;; (after! lsp-clangd
;;   (setq lsp-clients-clangd-args
;;         '("-j=2"
;;           "--malloc-trim"
;;           "--log=error"
;;           "--background-index"
;;           "--clang-tidy"
;;           "--completion-style=detailed"
;;           "--pch-storage=memory"
;;           "--header-insertion=never"
;;           "--header-insertion-decorators=0")))

;(add-hook! c-mode-common #'google-set-c-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (setq tide-format-options '(:indentSize 4
                              :tabSize 4
                              :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                              :placeOpenBraceOnNewLineForFunctions nil
                              :placeOpenBraceOnNewLineForControlBlocks nil)))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
(add-hook 'tsx-ts-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'rjsx-mode-hook #'setup-tide-mode)

;;(set-lookup-handlers! 'js2-mode :xref-backend #'xref-js2-xref-backend)
;; (use-package! nvm
;;   :after +Javascript-Npm
;;   :config
;;   (setq nvm-dir "/usr/share/nvm"))

;; (add-hook! (js2-mode web-mode) 'prettier-js-mode)
;;(add-hook! (js2-mode web-mode typescript-tsx-mode) #'add-node-modules-path)

;; Files (from https://github.com/elken/doom#marginalia)
;; (after! marginalia
;;   (setq marginalia-censor-variables nil)

;;   (defadvice! +marginalia--anotate-local-file-colorful (cand)
;;     "Just a more colourful version of `marginalia--anotate-local-file'."
;;     :override #'marginalia--annotate-local-file
;;     (when-let (attrs (file-attributes (substitute-in-file-name
;;                                        (marginalia--full-candidate cand))
;;                                       'integer))
;;       (marginalia--fields
;;        ((marginalia--file-owner attrs)
;;         :width 12 :face 'marginalia-file-owner)
;;        ((marginalia--file-modes attrs))
;;        ((+marginalia-file-size-colorful (file-attribute-size attrs))
;;         :width 7)
;;        ((+marginalia--time-colorful (file-attribute-modification-time attrs))
;;         :width 12))))

;;   (defun +marginalia--time-colorful (time)
;;     (let* ((seconds (float-time (time-subtract (current-time) time)))
;;            (color (doom-blend
;;                    (face-attribute 'marginalia-date :foreground nil t)
;;                    (face-attribute 'marginalia-documentation :foreground nil t)
;;                    (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
;;       ;; 1 - log(3 + 1/(days + 1)) % grey
;;       (propertize (marginalia--time time) 'face (list :foreground color))))

;;   (defun +marginalia-file-size-colorful (size)
;;     (let* ((size-index (/ (log (+ 1 size)) 7.0 10.0))
;;            (color (if (< size-index 10000000) ; 10m
;;                       (doom-blend 'orange 'green size-index)
;;                     (doom-blend 'red 'orange (- size-index 1)))))
;;       (propertize (file-size-human-readable size) 'face (list :foreground color)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info pages (from https://github.com/elken/doom#info-pages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :custom (Info-selection . #'info-colors-fontify-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline (from https://github.com/elken/doom#modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! doom-modeline
  (setq all-the-icons-scale-factor 1.1
        ;; auto-revert-check-vc-info t
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p)
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-github t
        doom-modeline-github-interval 60
        doom-modeline-vcs-max-length 60)
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (doom-modeline-def-modeline 'main
    '(bar modals workspace-name window-number persp-name buffer-position selection-info buffer-info matches remote-host debug vcs matches)
    '(github mu4e grip gnus misc-info repl " ")))

(dolist
    (entry '(("\\.h\\'" . c++-mode)
             ("\\.tsx?\\'" . nil)
             ("\\.ts\\'" . typescript-mode)
             ("\\.tsx\\'" . web-mode)
             ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode)
             ("\\.\\(cc\\|hh\\)\\'" . c++-mode)
             ("\\.py[iw]?\\'" . python-ts-mode)
             ("\\.css\\'" . css-mode)
             ("\\.json\\'" . json-ts-mode)
             ("\\.cmake\\'" . cmake-mode)
             ("\\CMakeLists\\.txt\\'" . cmake-mode)
             ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-ts-mode)
             ("\\.rs\\'" . rust-ts-mode)))
  (add-to-list 'auto-mode-alist entry))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown (+grip)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package grip-mode
;;   :ensure t
;;   :bind (:map markdown-mode-command-map
;;               ("g" . grip-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck (Syntax checking)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! flycheck
  (defun my/flycheck-locate-config-file-xdg-config (filename checker)
    "Locate a configuration `FILENAME' within the XDG_CONFIG_HOME directory.
The subdirectory in which to look for `FILENAME' is suffix for the `CHECKER'.
For example, for the checker `python-mypy', this function will look into the
directory \"XDG_CONFIG_HOME/mypy/\"."
    (let* ((checker-name (symbol-name checker))
           (subdirectory
            (when (string-match "-\\(.*\\)$" checker-name)
              (match-string 1 checker-name))))
      (expand-file-name
       filename
       (file-name-concat (xdg-config-home) subdirectory))))
  (add-to-list 'flycheck-locate-config-file-functions #'my/flycheck-locate-config-file-xdg-config))

;; (defun +markdown-flyspell-word-p ()
;;     "Return t if point is on a word that should be spell checked.

;; Return nil if on a link url, markup, html, or references."
;;     (let ((faces (ensure-list (get-text-property (point) 'face))))
;;       (or (and (memq 'font-lock-comment-face faces)
;;                (memq 'markdown-code-face faces))
;;           (not (cl-loop with unsafe-faces = '(markdown-reference-face
;;                                               markdown-url-face
;;                                               markdown-markup-face
;;                                               markdown-comment-face
;;                                               markdown-html-attr-name-face
;;                                               markdown-html-attr-value-face
;;                                               markdown-html-tag-name-face
;;                                               markdown-code-face)
;;                         for face in faces
;;                         if (memq face unsafe-faces)
;;                         return t)))))

;; (after! flyspell-mode
;;   (set-flyspell-predicate! '(markdown-mode gfm-mode)
;;                            #'+markdown-flyspell-word-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arxiv-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! arxiv-mode
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chrome.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun load-chrome-debug ()
;;         (load! (expand-file-name "chrome/chrome.el" local-repository-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docstring generator using abo-abo's auto-yas-snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable `docstr' inside these major modes.
;(setq! yas-global-mode 1)

;; (defun moo-doxygen ()
;;   "Generate a doxygen yasnippet and expand it with `aya-expand'.
;; The point should be on the top-level function name."
;;   (interactive)
;;   (move-beginning-of-line nil)
;;   (let ((tag (semantic-current-tag)))
;;     (unless (semantic-tag-of-class-p tag 'fun () ction)
;;       (error "Expected function, got %S" tag))
;;     (let* ((name (semantic-tag-name tag))
;;            (attrs (semantic-tag-attributes tag))
;;            (args (plist-get attrs :arguments))
;;            (ord 1))
;;       (setq aya-current
;;             (format
;;              "/**
;; * $1
;; *
;; %s
;; * @return $%d
;; */
;; "
;;              (mapconcat
;;               (lambda (x)
;;                 (format "* @param %s $%d"
;;                         (car x) (incf ord)))
;;               args
;;               "\n")
;;              (incf ord)))
;;       (aya-expand))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tb-dev.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(use-package! tb-dev
;  :defer t
;  :after projectile)

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

;;; config.el ends here

;; Local Variables:
;; +emacs-lisp-non-package-mode: t
;; End:
