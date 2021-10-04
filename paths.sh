#!/usr/bin/env sh

dir="/usr/share/paths"

paths() {
  if [ $1 = "go" ]; then
    cd "$($dir/paths-exe go $2)";
  else
    $dir/paths-exe "$@";
  fi
}
