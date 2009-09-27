#!/usr/bin/env python
# -*- coding: utf-8 -*-

import threading
import matplotlib
import serial

matplotlib.use('GTKAgg')
import gobject
import matplotlib.pyplot as plt

figure = plt.figure()
axes = figure.add_subplot(111)
x_points = range(1000)
y_points = [0 for i in range(1000)]
line = axes.plot(x_points, y_points, 'b')[0]

class Device(threading.Thread):
    def __init__(self):
        threading.Thread.__init__(self)
        self.value = 0
        self.accumulator = []
        self.run_on = False
        #self.source = serial.Serial('/dev/ttyUSB0', 9600)
        self.source = open('foo', 'r')

    def run(self):
        self.run_on = True

        while self.run_on:
            c = self.source.read(1)
            if c == '\n':
                try:
                    self.value = int(''.join(self.accumulator))
                    self.accumulator = []
                except ValueError:
                    pass
            elif c == '\r':
                pass
            else:
                self.accumulator.append(c)

    def stop(self):
        self.run_on = False

def update_graph(source):
    value = source.value
    for i in xrange(10):
        y_points.pop(0)
        y_points.append(value)
        line.set_ydata(y_points)
    axes.set_ylim(ymax=1024)
    axes.set_xlim(xmax=1000)
    figure.canvas.draw()
    return True

def main():
    source = Device()
    source.start()
    gobject.idle_add(update_graph, source)
    plt.show()
    source.stop()

if __name__ == '__main__':
    main()
