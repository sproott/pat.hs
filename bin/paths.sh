#!/usr/bin/env sh

DIR="/usr/share/paths"

paths2() {
  if [ "$1" = "go" ]; then
    echo "$2"
    OUTPUT=$($DIR/paths go "$2");
    if [ $? -ne 0 ]; then
      echo "$OUTPUT"
      return 1
    fi
    cd "$OUTPUT"
  else
    $DIR/paths "$@";
  fi
}
