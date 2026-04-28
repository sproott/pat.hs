#!/usr/bin/env zsh

dir="/usr/share/paths"

paths() {
  if [[ "$1" == "go" ]]; then
    local output
    output=$($dir/paths go "$2")
    if [[ $? -ne 0 ]]; then
      echo "$output"
      return 1
    fi
    cd "$output"
  else
    $dir/paths "$@"
  fi
}
