;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jean-Francois Arbour"
      user-mail-address "jf.arbour@gmail.com")
(setq org-directory "~/org/")
(setq Info-additional-directory-list '("~/.local/share/info/"))
(setq display-line-numbers-type t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-error-top-bottom t
      next-screen-context-lines 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq display-time-day-and-date t
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
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
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

;; To automatically update the rss feeds when opening elfeed
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
;; Setup elfeed to show us entries from the last month that are unread
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! org-default-notes-file (concat org-directory "/notes.org"))
;; (add-to-list 'org-todo-keywords '(:type "seq(N)"))
;; (after! org  org-babel-do-load-languages
;;  'org-babel-load-languages '((C   . t)
;;                              (C++ . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
