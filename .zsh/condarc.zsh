#!/usr/bin/env zsh

# Virtualenv/Conda prompt extensions
# Conda support
setopt PROMPT_SUBST

# Add any commands which depend on conda here
lazy_conda_aliases=('python' 'conda')

load_conda() {
  for lazy_conda_alias in $lazy_conda_aliases
  do
    unalias $lazy_conda_alias
  done

  __conda_prefix="/opt/miniconda3" # Set your conda Location

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/miniconda3/etc/profile.d/conda.sh"
        print(f"Loading profile.d/conda.sh")
    else
        export PATH="/opt/miniconda3/bin:$PATH"
        "./$PATH/"
    fi
fi
unset __conda_setup

if [ -f "/opt/miniconda3/etc/profile.d/mamba.sh" ]; then
    . "/opt/miniconda3/etc/profile.d/mamba.sh"
fi
# >>> conda initialize >>>

unfunction load_conda
}

for lazy_conda_alias in $lazy_conda_aliases
do
  alias $lazy_conda_alias="load_conda && $lazy_conda_alias"
done

function get_conda_env() {
    echo $CONDA_DEFAULT_ENV
}

function show_conda_env () {
    REPLY=""
    if [ "$CONDA_DEFAULT_ENV" != "base" ]; then
        REPLY="${CONDA_PROMPT_MODIFIER}"
    fi
}
grml_theme_add_token conda-env -f show_conda_env
zstyle ':prompt:grml:left:setup' items rc conda-env virtual-env change-root user at host path vcs percent
