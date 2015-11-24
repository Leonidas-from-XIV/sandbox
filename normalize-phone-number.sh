#!/bin/zsh
set -u

mypath=$(dirname "$0:A")
normalized=$(xmlstarlet transform "$mypath/normalize-phone-number.xsl" "$1")
echo $normalized | uniq
