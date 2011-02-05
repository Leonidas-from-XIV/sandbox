#!/usr/bin/env python

# http://pyusb.sourceforge.net/docs/1.0/tutorial.html
# http://lxr.linux.no/#linux+v2.6.37/Documentation/usb/usbmon.txt
import usb.core

# https://usb-ids.gowdy.us/read/UD/046d/c52b
LOGITECH = 0x046d
UNIFY = 0xc52b

receiver = usb.core.find(idVendor=LOGITECH, idProduct=UNIFY)
if receiver is None:
    print("No device detected, please make sure you have "
            "plugged the Unify receiver in")

print(receiver)
