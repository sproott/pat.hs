#!/usr/bin/env sh

dir="/usr/share/paths"

paths() {
  if [ $1 = "go" ]; then
    cd "$($dir/paths go $2)";
  else
    $dir/paths "$@";
  fi
}
