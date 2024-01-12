#!/usr/bin/env sh

# Pipe stdin into an emacs buffer for editing

if [ -t 0 ]; then
    emacsclient -n "$@"
else
    # stdin has data
    TMP_FILE=$(mktemp -q --tmpdir emacs-edit.XXXXXXXX || exit 1)

    cat > $TMP_FILE

    # Set trap to clean up file
    trap 'rm -f -- $TMP_FILE; trap - EXIT; exit' EXIT INT HUP

    emacs-eval <<-EOF
    (progn
      (let ((dir default-directory))
        (find-file "TMP_FILE")
        (setq default-directory dir)
        (set-visited-file-name nil)
        (rename-buffer "*stdin*" t)))
EOF
fi
