"""
A simple example of an animated plot using a gtk backend
"""
import random
import matplotlib
import serial
import threading

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
        #self.source = serial.Serial('/dev/ttyUSB0', 9600)
        self.source = open('foo', 'r')

    def run(self):
        while True:
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

def update_graph(source):
    #value = random.randrange(0, 1024, 1)
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

if __name__ == '__main__':
    main()
