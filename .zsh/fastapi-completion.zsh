#compdef fastapi

_fastapi_completion() {
  eval $(env _TYPER_COMPLETE_ARGS="${words[1,$CURRENT]}" _FASTAPI_COMPLETE=complete_zsh fastapi)
}

compdef _fastapi_completion fastapi
