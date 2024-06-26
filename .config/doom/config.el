;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'dash)

(setq! user-full-name "Jean-Francois Arbour"
       user-mail-address "jf.arbour@gmail.com")

;(load! "/home/jfa/projects/echo-crafter/echo-crafter.el")

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
(defun my-mark-defun-fix (orig-fun &rest args)
  "Wrapper for mark-defun to exclude the line above the current defun."
  (apply orig-fun args)
  (goto-char (mark))
  (beginning-of-defun))

(advice-add 'mark-defun :around #'my-mark-defun-fix)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! doom-font (font-spec :family "JetBrains Mono" :size 16 :weight 'light)
       doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14)
       doom-theme 'doom-dracula)

(setq envrc-direnv-executable "/usr/bin/direnv")
(setq inhibit-x-resources nil)
(setq browse-url-browser-function 'browse-url-chrome)
(setq browse-url-chrome-program "/usr/bin/google-chrome-stable")


;; Map \\[execute-extended-command] to C-c C-m and C-x C-m
;; since it's faster than M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; Map \\[backward-kill-word] to C-w (the default in bash)
(global-set-key (kbd "C-w") 'backward-kill-word)

;; Remap \\[kill-region] to C-x C-k (it was C-w)
(global-set-key (kbd "C-x C-k") 'kill-region)


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

;; Asynchronously highlights both files and directories based
;; on their git status
;(setq +treemacs-git-mode 'deferred)

;; (after! python-mode
;;   :config
;;   (setq flycheck-python-pyright-executable "/usr/bin/pyright"))

;; (add-hook!
;;  :append
;;  'doom-after-init-hook '(global-flycheck-mode,
;;                          global-eldoc-mode,
;;                          global-company-mode,
;;                          global-auto-revert-mode,
;;                          global-visual-line-mode,
;;                          global-subword-mode,
;;                          global-hl-line-mode,
;;                          global-npm-mode,
;;                          global-direnv-mode,
;;                          global-copilot-mode,
;;                          global-font-lock-mode,
;;                          global-goto-address-mode
;;                          ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restclient http helpers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! know-your-http-well
  :after company-restclient)

;; (use-package! flycheck
;;   :init
;;   (setq flycheck-pylintrc "~/.config/pylint/pylintrc")
;;   :config
;;   (setq flycheck-check-syntax-automatically t)
;;   (setq flycheck-python-pyright-executable "/usr/bin/pyright"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github Copilot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accept completion from copilot and fallback to company
;; (use-package! copilot
;;   :hook '(prog-mode copilot-mode)
;;   :bind '(:map copilot-completion-map
;;           ("<tab>" . 'copilot-accept-completion)
;;           ("TAB" . 'copilot-accept-completion)
;;           ("C-TAB" . 'copilot-accept-completion-by-word)
;;           ("C-<tab>" . 'copilot-accept-completion-by-word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codeium
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! codeium
  :init
  ;; enable in prog-mode
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  ;(add-hook! prog-mode-hook (setq-local completion-at-point-functions '(codeium-completion-at-point)))
  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  (add-hook! emacs-start-hook (run-with-timer 0.1 nil #'codeium-init))
  (setq use-dialog-box nil)
  (setq codeium-mode-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(after! company
  :defer 0.1
  :config
  (setq-default
   company-idle-delay 0.05
   company-require-match nil
   company-minimum-prefix-length 0

   ;; get only previews
   company-frontends '(company-preview-frontend)
   ;; also get a dropdown
   ;; company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communicate with LLMs using ellm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load (expand-file-name "~/projects/emacs/ellm/ellm.el"))
;; (require 'ellm)
;; (global-ellm-mode)

(defun get-ellm-api-key-from-password-store ()
  "Setup the API keys for the ellm package."
  (password-store-get (format "%s/ellm_api_key" (symbol-name ellm-provider))))

(defun get-openai-api-key ()
  "Get the OpenAI API key from the password store."
  (password-store-get "openai/ellm_api_key"))
(defun get-anthropic-api-key ()
  "Get the OpenAI API key from the password store."
  (password-store-get "anthropic/ellm_api_key"))
(defun get-groq-api-key ()
  "Get the OpenAI API key from the password store."
 (password-store-get "groq/ellm_api_key"))
(defun get-mistral-api-key ()
  "Get the OpenAI API key from the password store."
  (password-store-get "mistral/ellm_api_key"))

(defun my-ellm-password-store-path (provider)
  "Get the password store path for the provider."
  (format "%s/ellm_api_key" (symbol-name provider)))

(use-package! ellm
  :custom
  (ellm-password-store-path-by-provider #'my-ellm-password-store-path)
  (ellm-get-api-key #'ellm-get-api-key-from-password-store)
  :config
  (global-ellm-mode)
  (ellm-start-server)
  (ellm--setup-persistance))

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
;; (defun +insert-random-uuid ()
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
                                           :run "npm run dev")
  (setq projectile-indexing-method 'alien))


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

;(keymap-set vertico-map "?" #'minibuffer-completion-help)
;(keymap-set vertico-map "RET" #'minibuffer-force-complete-and-exit)
; (keymap-set vertico-map "TAB" #'minibuffer-complete)
;(keymap-set vertico-map "M-o" #'consult-multi-occur)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Consult instead of isearch
;(map! "C-s" #'consult-line)
;(map! "C-r" #'consult-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(setq company-transformers nil)

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
(use-package! org-protocol-capture-html
  :config
  (setq org-protocol-capture-html-templates
        '(("w" "Web site" entry
           (file "")
           "* %a :website:\n\n%U %?\n\n%:initial"))))

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

;(setq! flycheck-python-pylint-executable "pyright"
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
;;(set-lookup-handlers! 'js2-mode :xref-backend #'xref-js2-xref-backend)
(use-package! nvm
  :after +Javascript-Npm
  :config
  (setq nvm-dir "/usr/share/nvm"))

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
    (entry '(("\\.h\\'" . c++-ts-mode)
             ("\\.tsx?\\'" . nil)
             ("\\.ts\\'" . typescript-ts-mode)
             ("\\.tsx\\'" . tsx-ts-mode)
             ("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-ts-mode)
             ("\\.\\(cc\\|hh\\)\\'" . c++-ts-mode)
             ("\\.py[iw]?\\'" . python-ts-mode)
             ("\\.css\\'" . css-ts-mode)
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
;; Flycheck (Word spelling checker)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(toggle-debug-on-error)
