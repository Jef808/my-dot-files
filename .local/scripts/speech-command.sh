#!/usr/bin/env sh

$HOME/projects/echo-crafter/.venv/bin/python $HOME/projects/echo-crafter/speech-to-text/speech_to_text.py | /home/jfa/projects/echo-crafter/make-prompt/shell.py | xargs -I {} xdotool type "{}"
