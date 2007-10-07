#!/usr/bin/env python
from __future__ import with_statement
from datetime import datetime, timedelta

offset = +9
delta = timedelta(seconds=offset)

out = file('infile.srt.out', 'w')

with file('infile.srt') as subfile:
    for line in subfile:
        if line.count(':') == 4:
            newpoints = list()
            for timepoint in line.split()[::2]:
                timechunks = timepoint.split(',')
                point = datetime.strptime(timechunks[0], "%H:%M:%S")
                point += delta
                newpoint = "%s,%s" % (point.strftime("%H:%M:%S"), timechunks[1])
                newpoints.append(newpoint)
            newline = ' --> '.join(newpoints) + '\n'
            out.write(newline)
        else:
            out.write(line)

out.close()
