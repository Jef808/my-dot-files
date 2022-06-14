;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;;
(setq! user-full-name "Jean-Francois Arbour"
       user-mail-address "jf.arbour@gmail.com"
       org-directory "~/org/"
       Info-additional-directory-list '("/home/jfa/.local/share/info/")
       display-line-numbers-type t)

(defvar my-local-dir "~/.local/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! scroll-error-top-bottom t
      next-screen-context-lines 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sbcl and Slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! inferior-lisp-program "sbcl")
;; (use-package! slime
;;   :commands #'slime-setup
;;    (load! (expand-file-name "~/quicklisp/slime-helper.el")))
;; (after! slime
;;     (slime-setup '(slime-company)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! display-time-day-and-date t
       display-time-24hr-format t
       display-time-mode t
       display-battery-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq! lsp-enable-indentation nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cc-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lsp-clients-clangd-args '("-j=3"
                                "--enable-config"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=iwyu"))
                                ;;"--header-insertion-decorators=0"))

;(after! lsp-clangd (set-lsp-priority! 'clangd 2))
(defun patch-lineup-inclass nil
  (defun +cc-c++-lineup-inclass (langelem)
    "Indent inclass lines one level further than access modifier keywords."
    (and (eq major-mode 'c++-mode)
         (or (assoc 'access-label c-syntactic-context)
             (save-excursion
               (save-match-data
                 (re-search-backward
                  "\\(?:p\\(?:ublic\\|r\\(?:otected\\|ivate\\)\\)\\)"
                  (c-langelem-pos langelem) t))))
         '+)))
(add-to-list 'c-mode-hook 'patch-lineup-inclass)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'python-mode-hook
;;              (add-to-list 'lsp-clients-python-library-directories "~/.emacs.d/.local/etc/"))
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
  (setq-default TeX-master nil)
  )

;; The global bibliography file
(setq! citar-bibliography '("~/bib/references.bib")
       citar-library-paths '("~/math/library")
       citar-notes-paths '("~/bib/notes"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                      (:session . "py"))
       org-default-notes-file (concat org-directory "/notes.org"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode hooks         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-hook! 'org-mode-hook #'org-modern-mode)
(add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode latex         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO Maybe create a minor mode for this instead?
(setq! yas-default-user-snippets-dir +snippets-dir)
;;(yas-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RSS feed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread")
  (add-hook! elfeed-search-mode-hook #'elfeed-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Email access configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! mu4e
  (setq! sendmail-program (executable-find "msmtp")
         send-mail-function #'smtpmail-send-it
         message-sendmail-f-is-evil nil
         message-sendmail-extra-arguments '("--read-envelope-from")
         message-send-mail-function #'message-send-mail-with-sendmail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! flycheck-markdown-markdownlint-cli-executable
       (file-name-concat my-local-dir "bin/marked"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modus themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! modus-themes-mode-line '(accented borderless padded)
       modus-themes-region '(bg-only)  ; opt: accented no-extend
       modus-themes-paren-match '(bold)
       modus-themes-bold-constructs t
       modus-themes-italic-constructs t
       modus-themes-org-blocks '(gray-background)  ; tinted-background
       ;modus-themes-syntax '(alt-syntax)
       )
;; Themes need to be reloaded for the non-default variable
;; values to kick in
;(load-theme modus-operandi t)
;(load-theme modus-vivendi t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
