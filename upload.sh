#!/bin/bash

# Uploads one or more source files to a code.world installation

set -e

url="$1"
shift
mode="$1"
shift

if [ -z "$url" -o -z "$mode" ]
then
  echo "Usage: $0 http://code.world.url/ [haskell|codeworld] [source.hs ...]"
  exit 1
fi

for file in "$@"
do
    echo "Uploading $file..."
    json="$(curl -S -s -F "mode=$mode" -F "source=<$file" ${url}compile)"
    dhash="$(echo "$json" | jq -r .dhash)"
    hash="$(echo "$json" | jq -r .hash)"
    if [  "$mode" = "codeworld" ]
    then
      curl="$url#$hash"
    else
      curl="$url$mode#$hash"
    fi
    durl="${url}run.html?mode=$mode&dhash=$dhash"
    echo "Code at $curl"
    echo "Opening $durl"
    gnome-open "$durl"
done

