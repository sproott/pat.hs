_paths() {
  local CMDLINE
  local IFS=$'\n'
  CMDLINE=(--bash-completion-index $COMP_CWORD)

  for arg in ${COMP_WORDS[@]}; do
      CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
  done

  COMPREPLY=( $(/usr/share/paths/paths-exe "${CMDLINE[@]}") )
}

complete -o filenames -F _paths paths
