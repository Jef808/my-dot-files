;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! user-full-name "Jean-Francois Arbour"
       user-mail-address "jf.arbour@gmail.com"
       info-additional-directory-list '((expand-file-name"~/.local/share/info/")))


(add-load-path!
   "editor/formatting/"
   "editor/file_templates/"
   "snippets/")

(setq doom-font (font-spec :family "JetBrainsMono" :size 12 :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc extra loads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load! (expand-file-name (concat doom-user-dir "files/uuid.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't display line number by default
;; (makes a big difference in performance)
(setq display-line-numbers-type nil)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq scroll-error-top-bottom t
      next-screen-context-lines 4)

(after! windmove
  (load! "buffer-move.el" (expand-file-name "buffers/" doom-user-dir)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Manual completion
;; (make time before automatic completion prompts infinite)
;; (after! company
;;   (setq company-idle-delay nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lsp-eslint-code-action-disable-rule-comment t
       lsp-eslint-code-action-show-documentation t)

;; Disable some annoying lsp features
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil))
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil   ; flycheck instead
        lsp-ui-doc-enable nil))      ; C-c c k instead

;; clangd client parameters
(setq! clangd-args '("-j=2"
                     "--log=verbose"
                     "--all-scopes-completion"
                     "--completion-parse=auto"
                     "--header-insertion-decorators"
                     "--malloc-trim"
                     "--pch-storage=memory"
                     "--query-driver=/usr/bin/g++"
                     "--enable-config"
                     "--background-index"
                     "--clang-tidy"
                     "--completion-style=detailed"
                     "--header-insertion=iwyu"
                     "--limit-references=500"
                     "--limit-results=50"
                     "--offset-encoding=utf-8"))
(setq! lsp-clients-clangd-args clangd-args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq magit-repository-directories '(("~/projects" . 2))
      magit-save-repository-buffers nil
      magit-inhibit-save-previous-winconf t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar private-file-templates-dir
  (expand-file-name "snippets/templates/" doom-user-dir)
  "The path to a directory of yasnippet folders to use for file templates.")
(use-package! yasnippets
  :defer nil
  :config
  (add-to-list 'yas-snippet-dirs 'private-file-templates-dir 'append #'eq)
  (setq! yas-default-user-snippets-dir +snippets-dir))
;; TODO Maybe create a minor mode for this instead?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! web-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
;; (use-package! lsp-tailwindcss)

;;(use-package! lsp-volar)

(use-package! npm-mode)

(use-package! tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sbcl and Slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! sly
  :config
  (setq sly-path (expand-file-name "~/.local/repos/sly/"))
  ;; These two paths were still pointing to the emacs folder
  ;; (setq sly-macrostep--path (file-name-concat sly-path "contrib/"))
  ;; (setq sly-repl-ansi-color--path (file-name-concat sly-path "contrib/"))
  (add-to-list 'load-path 'sly-path)
  (require 'sly-autoloads)
  (setq inferior-lisp-program (expand-file-name "~/.local/bin/sbcl"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When running emacsclient as daemon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun client-save-kill-emacs(&optional display)
    " This is a function that can bu used to save buffers and
shutdown the emacs daemon. It should be called using
emacsclient -e '(client-save-kill-emacs)'.  This function will
check to see if there are any modified buffers, active clients
or frame.  If so, an x window will be opened and the user will
be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

    ; Check if there are modified buffers, active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
					(> (length (frame-list)) 1)
				       ))

    ; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
	(message "Initializing x windows system.")
	(x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

    ; Save the current frame.
    (setq new-frame (selected-frame))


    ; When displaying the number of clients and frames:
    ; subtract 1 from clients (this client).
    ; subtract 2 from frames (the frame just created and the default frame.)
    (when (or (not active-clients-or-frames)
	       (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))

      ; If the user quits during the save dialog then don't exit emacs.
      ; Still close the terminal though.
      (let((inhibit-quit t))
             ; Save buffers
	(with-local-quit
	  (save-some-buffers))

	(if quit-flag
	  (setq quit-flag nil)
          ; Kill all remaining clients
	  (progn
	    (dolist (client server-clients)
	      (server-delete-client client))
		 ; Exit emacs
	    (kill-emacs)))
	))

    ; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )


(defun modified-buffers-exist()
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
		 (buffer-modified-p buffer)
		 (not (buffer-base-buffer buffer))
		 (or
		  (buffer-file-name buffer)
		  (progn
		    (set-buffer buffer)
		    (and buffer-offer-save (> (buffer-size) 0))))
		 )
	(setq modified-found t)
	)
      )
    modified-found
    )
  )

;; (after! slime
;;   (load! (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (slime-setup '(slime-company))
;;   ;; (add-hook! 'slime-mode-hook :append '(sly-editing-mode))
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! display-time-day-and-date t
       display-time-24hr-format t
       display-time-mode t
       display-battery-mode t
       doom-modeline-github t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cc-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! flycheck-cppcheck-standards "--std=c++17")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(defun ++no-lsp-formatting ()
  "Turn off lsp formatting."
  (setq! +format-with-lsp nil))
(add-hook! c++-mode '++no-lsp-formatting)
(use-package! google-c-style
  :after-call c-mode-common-hook
  :config (progn (google-set-c-style) (google-make-newline-indent)))

;;(set-eglot-client! 'cc-mode clangd-args)
                                ;;"--header-insertion-decorators=0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! python-mode
  (setq conda-env-autoactivate-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq +workspaces-on-switch-project-behavior t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'projectile-globally-ignored-directories "*build")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(after! reftex
  ;; Enables the use of the 'subfile' package with reftex parsing
  (add-to-list 'reftex-include-file-commands "subfile")
  ;; Fallback to the global bibliography file when no .bib file is available
  (add-to-list 'reftex-default-bibliography "~/bib/references.bib")
  ;; Enables referencing using the amsmath's \eqref command
  (setq! reftex-label-alist '(AMSTeX)
         ;;bibtex-dialect 'BibTeX
         bibtex-maintain-sorted-entries t
         bibtex-entry-delimiters "\n\n")
  ;; Makes it so that AucTeX asks for a master file automatically
  (setq-default TeX-master nil))
  

;; The global bibliography file
(setq! citar-bibliography '("~/bib/references.bib")
       citar-library-paths '("~/math/lib" "~/cs/lib")
       citar-notes-paths '("~/bib/notes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-directory "~/org/"
      org-roam-directory (concat org-directory "roam/")
      org-roam-db-location (concat org-directory ".org-roam.db")
      org-roam-dailies-directory "journal/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-agenda-files org-directory)

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

;; Use org-modern styles
(add-hook! 'org-mode-hook #'org-modern-mode
           'org-agenda-finalize-hook #'org-modern-agenda)

(after! org
  (setq org-startup-folded 'show2levels
        org-ellipsis " [...] "
        org-capture-templates
        '(("t" "todo" entry (file+headline "todo.org" "Unsorted")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          ("d" "deadline" entry (file+headline "todo.org" "Schedule")
           "* [ ] %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("s" "schedule" entry (file+headline "todo.org" "Schedule")
           "* [ ] %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a"
           :prepend t)
          ("c" "check out later" entry (file+headline "todo.org" "Check out later")
           "* [ ] %?\n%i\n%a"
           :prepend t)
          ("l" "ledger" plain (file "ledger.gpg")
           "%(+beancount/clone-transaction)"))))

(after! org-roam
  (setq org-roam-capture-templates
        `(("n" "note" plain
           ,(format "#+title: ${title}\n%%[%s/template/note.org]" org-roam-directory)
           :target (file "note/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("r" "thought" plain
           ,(format "#+title: ${title}\n%%[%s/template/thought.org]" org-roam-directory)
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("t" "topic" plain
           ,(format "#+title: ${title}\n%%[%s/template/topic.org]" org-roam-directory)
           :target (file "topic/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("c" "contact" plain
           ,(format "#+title: ${title}\n%%[%s/template/contact.org]" org-roam-directory)
           :target (file "contact/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("p" "project" plain
           ,(format "#+title: ${title}\n%%[%s/template/project.org]" org-roam-directory)
           :target (file "project/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("i" "invoice" plain
           ,(format "#+title: %%<%%Y%%m%%d>-${title}\n%%[%s/template/invoice.org]" org-roam-directory)
           :target (file "invoice/%<%Y%m%d>-${slug}.org")
           :unnarrowed t)
          ("f" "ref" plain
           ,(format "#+title: ${title}\n%%[%s/template/ref.org]" org-roam-directory)
           :target (file "ref/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("w" "works" plain
           ,(format "#+title: ${title}\n%%[%s/template/works.org]" org-roam-directory)
           :target (file "works/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("s" "secret" plain "#+title: ${title}\n\n"
           :target (file "secret/%<%Y%m%d%H%M%S>-${slug}.org.gpg")
           :unnarrowed t))
        ;; Use human readable dates for dailies titles
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%B %d, %Y>\n\n")))))

(after! org-roam
  ;; Offer completion for #tags and @areas separately from notes.
  (add-to-list 'org-roam-completion-functions #'org-roam-complete-tag-at-point)

  ;; Automatically update the slug in the filename when #+title: has changed.
  (add-hook 'org-roam-find-file-hook #'org-roam-update-slug-on-save-h)

  ;; Make the backlinks buffer easier to peruse by folding leaves by default.
  (add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)

  ;; List dailies and zettels separately in the backlinks buffer.
  (advice-add #'org-roam-backlinks-section :override #'org-roam-grouped-backlinks-section)

  ;; Open in focused buffer, despite popups
  (advice-add #'org-roam-node-visit :around #'+popup-save-a)

  ;; Make sure tags in vertico are sorted by insertion order, instead of
  ;; arbitrarily (due to the use of group_concat in the underlying SQL query).
  (advice-add #'org-roam-node-list :filter-return #'org-roam-restore-insertion-order-for-tags-a)

  ;; Add ID, Type, Tags, and Aliases to top of backlinks buffer.
  (advice-add #'org-roam-buffer-set-header-line-format :after #'org-roam-add-preamble-a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs everywhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! emacs-everywhere
  ;; Easier to match with a bspwm rule:
  ;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
  (setq emacs-everywhere-frame-name-format "emacs-anywhere")

  ;; Use local tempdir so that emacsclient has necessary permissions
  (setq emacs-everywhere-file-dir "/home/jfa/tmp/")
  ;; The modeline is not useful to me in the popup window. It looks much nicer
  ;; to hide it.
  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

  ;; Semi-center it over the target window, rather than at the cursor position
  ;; (which could be anywhere).
  (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
    :override #'emacs-everywhere-set-frame-position
    (cl-destructuring-bind (x y width height)
        (emacs-everywhere-window-geometry window-info)
      (set-frame-position frame
                          (+ x (/ width 2) (- (/ width 2)))
                          (+ y (/ height 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode latex         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For python-mode
(set-lookup-handlers! 'python-mode
  :definition #'anaconda-mode-find-definitions
  :references #'anaconda-mode-find-references
  :documentation #'anaconda-mode-show-doc)
;; For rjsx-mode (javascript)
(set-lookup-handlers! 'js2-mode :xref-backend #'xref-js2-xref-backend)

(setq! lsp-typescript-npm (concat doom-user-dir "node_modules/typescript")
       lsp-typescript-tsdk (concat doom-user-dir "node_modules/typescript/lib")
       lsp-typescript-tsserver-plugin-paths '((concat doom-user-dir "node_modules/typescript/lib/node_modules/typescript/bin")))

(after! 'python-mode
  (setq conda-env-autoactivate-mode t)
  (setq conda-env-autoactivate-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSS feed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread")
  (add-hook! elfeed-search-mode-hook #'elfeed-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Email access configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-email-account! "[Gmail]"
  '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
    ;;(mu4e-drafts-folder     . "/bar.com/Drafts")
    (mu4e-trash-folder      . "/[Gmail]/Trash")
    (mu4e-refile-folder     . "/[Gmail]/All Mail")
    (smtpmail-smtp-user     . "jf.arbour@gmail.com"))
    ;;(user-mail-address      . "foo@bar.com")    ;; only needed for mu < 1.4
    ;;(mu4e-compose-signature . "---\nYours truly\nThe Baz"))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! flycheck-markdown-markdownlint-cli-executable
       (file-name-concat "bin/marked"))

;; (setq! grip-github-user "Jef808"
;;        grip-github-password "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modus themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! modus-themes-mode-line '(accented borderless padded)
       modus-themes-region '(bg-only)  ; opt: accented no-extend
       modus-themes-paren-match '(bold)
       modus-themes-bold-constructs t
       modus-themes-italic-constructs t
       modus-themes-org-blocks '(gray-background))  ; tinted-background
       ;modus-themes-syntax '(alt-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trace emacs messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice message (before who-said-that activate)
  "Find out who said that thing. and say so."
  (let ((trace nil) (n 1) (frame nil))
    (while (setq frame (backtrace-frame n))
      (setq n     (1+ n)
            trace (cons (cadr frame) trace)) )
    (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
    (ad-set-args 1 (cons trace (ad-get-args 1))) ))

(ad-disable-advice 'message 'before 'who-said-that)
(ad-update 'message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zooming in/out by controlling font size
(map! :desc "increase font size"    "M-="           #'doom/increase-font-size
      :desc "decrease font size"    "M--"           #'doom/decrease-font-size
      :desc "reset font size"       "C-M-+"         #'doom/reset-font-size
      :leader
      (:prefix-map ("b" . "buffer movements")
        :desc "Move current buffer to the left"  "h"  #'buffer-move-left
        :desc "Move current buffer up"           "k"  #'buffer-move-up
        :desc "Move current buffer to the right" "l"  #'buffer-move-right
        :desc "Move current buffer down"         "j"  #'buffer-move-down))
