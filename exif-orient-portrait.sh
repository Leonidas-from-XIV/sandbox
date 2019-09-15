#!/bin/sh
exec exiftool -Orientation="Rotate 270 CW" $@
