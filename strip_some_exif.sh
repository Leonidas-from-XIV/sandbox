#!/bin/sh

EXIFTOOL=exiftool

show_help() {
	cat << EOF
Usage: ${0##*/} [-hgd] [FILE]
Strip potentially privacy-relevant EXIF information from files while preserving
technical information.
    -h          display this help and exit
    -g          preserve GPS data
    -d          dry-run. Just print options and exit
EOF
}

type $EXIFTOOL > /dev/null 2>&1 || {
	echo >&2 "This script requires exiftool but it's not installed."
	echo -n >&2 "exiftool is available in Debian as libimage-exiftool-perl"
	echo >&2 " and perl-image-exiftool in Arch Linux."
	echo >&2 "Aborting."
	exit 1;
}

GPS_TAGS="-GPSLatitude -GPSLatitudeRef -GPSLongitude -GPSLongitudeRef \
	-GPSAltitude -GPSAltitudeRef -GPSDateStamp -GPSTimeStamp \
	-GPSDateTime -GPSTrack -GPSTrackRef -GPSSpeed -GPSSpeedRef \
	-GPSImgDirection -GPSImgDirectionRef -GPSPitch -GPSRoll"

IMAGE_TAGS="\
	-Make -Model -LensModel -ApertureValue -FlashSetting -FlashType \
	-FlashExposureComp -FlashMode -ManufacturersModelName -Orientation \
	-Software -ModifyDate -Copyright -ExposureTime -FNumber \
	-ExposureProgram -ISO -CreateDate -FocalLength -ISOSetting \
	-FocusPosition -FocusDistance -ExifImageWidth -ExifImageHeight \
	-ColorSpace -Rights -ImageWidth -ImageHeight -ImageSize \
	-ExposureProgram -ExposureMode -Lens -ShutterSpeedValue \
	-DepthOfField -FieldOfView -FocalLength -XResolution -YResolution"

PRESERVE_GPS=false
DRY_RUN=""

while getopts "ghd" opt; do
	case "$opt" in
		h)
			show_help
			exit 0
			;;
		g)
			PRESERVE_GPS=true
			;;
		d)
			DRY_RUN="echo"
			;;
	esac
done
# delete options found
shift "$((OPTIND-1))"

if [ $PRESERVE_GPS == "false" ]; then
	GPS_TAGS=""
fi

$DRY_RUN $EXIFTOOL -all= -tagsFromFile @ $IMAGE_TAGS $GPS_TAGS "$@"
