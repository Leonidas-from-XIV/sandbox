#!/bin/bash
ISO=$1
IN="$@"
FILES=("${IN[@]:1}")
exec exiftool -exif:iso=$ISO $FILES

