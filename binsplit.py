#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""Splits numbers in bit sequences"""

import datetime

def decode_bit(number, max_bits=6):
    bits_found = {}
    number = float(number)
    for bit in reversed([2**x for x in range(max_bits)]):
        if number / bit >= 1:
            bits_found[bit] = int(number / bit)
            number -= bit * bits_found[bit]
    return bits_found

def main():
    stamp = datetime.datetime.today()
    print stamp.hour, decode_bit(stamp.hour)
    print stamp.minute, decode_bit(stamp.minute)
    print stamp.second, decode_bit(stamp.second)

if __name__ == '__main__':
    main()
