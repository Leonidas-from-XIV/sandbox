#!/usr/bin/zsh
# maximum X/Y resolution of image
MAX_SIZE=2048

# this must be of proper size
WATERMARK=watermark.png

# percentage of alpha that is applied to watermark
ALPHA_BLEND=100

# how much from the padding from edge in X direction
PADDING_X=30

# how much from the padding from edge in Y direction
PADDING_Y=30

# END OF CONFIG #
setopt null_glob

force_overwrite=false

zparseopts -D -E -A Args -- f -force
if (( ${+Args[-f]} )) || (( ${+Args[--force]} )); then
	force_overwrite=true
fi

# must be after zparseopts
in_folder="$1"
out_folder="$2"

for inputfile in $in_folder/*.jpg $in_folder/*.JPG
do
	filename=$(basename "$inputfile")
	outputfile="$out_folder"/"$filename"
	if [[ -a "$outputfile" ]]; then
		if [[ $force_overwrite == false ]]; then
			echo "File exists, skipping"
			continue
		fi
	fi

	convert $inputfile \
		-scale "$MAX_SIZE"\> \
		$WATERMARK \
		-gravity southeast \
		-geometry +"$PADDING_X"+"$PADDING_Y" \
		-compose dissolve \
		-define compose:args="$ALPHA_BLEND",100 \
		-composite \
		$outputfile
done
