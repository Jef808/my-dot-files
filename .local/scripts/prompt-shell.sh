#!/usr/bin/env sh

/home/jfa/projects/echo-crafter/.venv/bin/python /home/jfa/projects/echo-crafter/make-prompt/shell.py | xargs -I {} xdotool type "{}"
