#!/usr/bin/env sh

# Execute elisp from stdin

if [ -t 0 ]; then
    sexp="($*)" || sexp="$(cat)"
    exec emacsclient -n -e "$sexp"
fi
