"""
A simple example of an animated plot using a gtk backend
"""
import random
import matplotlib
import serial
import threading

#ser = serial.Serial('/dev/ttyUSB0', 9600)

matplotlib.use('GTKAgg')
import gobject
import matplotlib.pyplot as plt

figure = plt.figure()
axes = figure.add_subplot(111)

value = ""
throwaway = 10

class Device(threading.Thread):
    pass

def animate():
    global value, throwaway
    x = np.arange(0, 1024, 1)
    #line, = ax.plot(x, np.sin(x))
    while True:
        c = ser.read(1)
        if c == '\n':
            try:
                if throwaway == 20:
                    value_int = int(value)
                    print 'integer_value:', value_int
                    line.set_ydata(value_int)
                    fig.canvas.draw()
                    throwaway = 0
                else:
                    throwaway += 1
                value = ""
            except ValueError:
                pass
        elif c == '\r':
            pass
        else:
            value += c

x_points = range(1000)
y_points = [0 for i in range(1000)]
line = axes.plot(x_points, y_points, 'b')[0]

def update_graph():
    value = random.randrange(0, 1024, 1)
    for i in xrange(10):
        y_points.pop(0)
        y_points.append(value)
        line.set_ydata(y_points)
    axes.set_ylim(ymax=1024)
    axes.set_xlim(xmax=1000)
    figure.canvas.draw()
    return True

#gobject.timeout_add(1, animate)
#gobject.timeout_add(50, enter)
gobject.idle_add(update_graph)
print 'showing'
plt.show()
