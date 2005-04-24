#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import optparse, subprocess, os
import gtk, pango, gobject

apicall = True
try:
    import win32api
except ImportError:
    apicall = False

try:
    import ctypes
except ImportError:
    if not apicall:
        raise ImportError('You need pywin32 or ctypes')

def exit(process):
    """Kills a process spawned by subprocess"""
    if apicall:
        win32api.TerminateProcess(int(process._handle), -1)
    else:
        ctypes.windll.kernel32.TerminateProcess(int(process._handle), -1)
    

def main():
    left = [0, 0, 5]
    cw = CountdownWindow(left)
    gtk.main()

class TimeoutError(Exception):
    pass

class CountdownWindow(object):
    def __init__(self, left):
        self.window = gtk.Window()
        # connect with the destroy event to allow closing
        self.window.connect('delete_event', self.delete_event)
        # set a title
        self.window.set_title('Countdown') 
        
        self.label = gtk.Label('00:00:00')
        self.label.modify_font(pango.FontDescription('30'))
        self.window.add(self.label)

        self.window.show_all()
        self.timeleft = left
        self.activate()
    
    def activate(self):
        cmd = ['python.exe', '-c', 'while 1: pass']
        self.process = subprocess.Popen(cmd)
        self.terminated = False
        self.label.set_text(self.list2time(self.timeleft))
        gobject.timeout_add(1000, self.update)
    
    def update(self):
        try:
            print 'decreasing'
            self.decrease_time()
            self.label.set_text(self.list2time(self.timeleft))
            return True
        except TimeoutError:
            print 'exiting'
            exit(self.process)
            self.terminated = True
            return False
    
    def delete_event(self, widget, event):
        """Quitting the window"""
        if not self.terminated:
            exit(self.process)
        gtk.main_quit()
        return False
    
    def list2time(self, lst):
        hours = ''
        if lst[0] < 9:
            hours += '0'
        hours += str(lst[0])
        
        minutes = ''
        if lst[1] < 9:
            minutes += '0'
        minutes += str(lst[1])
        
        seconds = ''
        if lst[2] < 9:
            seconds += '0'
        seconds += str(lst[2])
        
        return hours + ':' + minutes + ':' + seconds
    
    def decrease_time(self):
        """Decreases the time"""
        hours, minutes, seconds = self.timeleft
        if seconds == 0:
            if minutes == 0:
                if hours == 0:
                    # no hours left, no minutes, no seconds: exit
                    raise TimeoutError
                else:
                    # there is some hour
                    hours -= 1
                    minutes = 59
            else:
                minutes -= 1
            seconds = 59
        else:
            seconds -= 1
        self.timeleft = [hours, minutes, seconds]

if __name__ == '__main__':
    main()