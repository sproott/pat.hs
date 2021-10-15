#!/usr/bin/env sh

DIR="/usr/share/paths"

paths() {
  if [ "$1" = "go" ]; then
    OUTPUT="$($DIR/paths go $2)";
    if [ $? -ne 0 ]; then
      echo "$OUTPUT"
      return 1
    fi
    cd "$OUTPUT"
  else
    $DIR/paths "$@";
  fi
}
