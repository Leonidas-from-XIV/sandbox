#!/bin/sh

EXIFTOOL=exiftool

type $EXIFTOOL > /dev/null 2>&1 || {
	echo >&2 "This script requires exiftool but it's not installed."
	echo -n >&2 "exiftool is available in Debian as libimage-exiftool-perl"
	echo >&2 " and perl-image-exiftool in Arch Linux."
	echo >&2 "Aborting."
	exit 1;
}

$EXIFTOOL -all= -tagsFromFile @ \
	-Make -Model -LensModel -ApertureValue -FlashSetting -FlashType \
	-FlashExposureComp -FlashMode -ManufacturersModelName -Orientation \
	-Software -ModifyDate -Copyright -ExposureTime -FNumber \
	-ExposureProgram -ISO -CreateDate -FocalLength -ISOSetting \
	-FocusPosition -FocusDistance -ExifImageWidth -ExifImageHeight \
	-ColorSpace -Rights -ImageWidth -ImageHeight -ImageSize \
	-ExposureProgram -ExposureMode -Lens -ShutterSpeedValue \
	-DepthOfField -FieldOfView -FocalLength -XResolution -YResolution \
	"$@"
