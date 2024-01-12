(doom! :input
       :completion
       (company +childframe)
       (vertico +childframe)
       :ui

       doom
       


       hl-todo
       



       modeline
       

       ophints
       (popup +defaults)

       (treemacs
        +lsp)

       (vc-gutter +pretty)
       vi-tilde-fringe

       workspaces
       

       :editor

       file-templates
       fold
       






       snippets
       

       :emacs
       dired
       electric
       undo
       vc
       :term
       eshell
       vterm
       :checkers
       syntax
       spell
       :tools
       direnv
       (eval +overlay)
       lookup
       (lsp +peek)
       magit
       pdf
       tree-sitter
       :os
       :lang
       (cc +lsp +tree-sitter)
       common-lisp
       data
       emacs-lisp
       (json
        +lsp
        +tree-sitter)
       (javascript
        +lsp
        +tree-sitter)











       (org
        +noter
        +pretty
        +journal
	)

       (python
        +lsp
        +conda
        +tree-sitter)

       sh
       
       


       (web +lsp)
       (yaml +lsp +tree-sitter)


       :app
       everywhere




       :config

       (default +bindings +smartparens))
