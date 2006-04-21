#!/usr/bin/env python

import sys, codecs

ignorechars = (' ', '-', ',', '.', '\n', '')

class Codec(codecs.Codec):
    def encode(self, input, errors='strict'):
        words = splitwords(input.lower())
	latwords = [piglatinize(word) for word in words]
	lattext = ''.join(latwords)
        return (lattext, len(input))

    def decode(self, input, errors='strict'):
        words = splitwords(input.lower())
	normwords = [depiglatinize(word) for word in words]
	normtext = ''.join(normwords)
	return (normtext, len(input))

class StreamWriter(Codec, codecs.StreamWriter):
    pass

class StreamReader(Codec, codecs.StreamReader):
    pass

def isword(word):
    if word in ignorechars:
        return False
    else:
        return True

def piglatinize(word):
    if isword(word):
        new_word = "%s%say" % (word[1:], word[0])
        return new_word
    else:
        return word

def depiglatinize(word):
    if isword(word):
        old_word = "%s%s" % (word[-3], word[:-3])
        return old_word
    else:
        return word

def splitwords(input):
    split = []
    buffer = ''
    for char in input:
        if char in ignorechars:
	    if buffer != '':
	        split.append(buffer)

	    split.append(char)
	    buffer = ''
	else:
	    buffer += char

    split.append(buffer)

    return split
    

def getregentry():
    return (Codec().encode, Codec().decode, StreamReader, StreamWriter)

def piglatin(infile, outfile):
    #print splitwords(infile.read())
    print infile.read().encode('piglatin')
    #print infile.read().decode('piglatin')
    #print 'ixnay'.decode('piglatin')

if __name__ == '__main__':
    piglatin(sys.stdin, sys.stdout)
