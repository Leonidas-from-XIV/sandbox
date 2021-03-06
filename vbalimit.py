#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A VisualBoyAdvance time limiter.
Het VisualBoyAdvance from http://vba.ngemu.com/
Look on http://www.pdroms.de/ for _legal_ GBA ROMs."""

import sys, subprocess, os, pickle
import gtk, pango, gobject
import ctypes

progpath = r'C:\Programme\VBoy\VisualBoyAdvance.exe'

def exit(process):
    """Kills a process spawned by subprocess"""
    ctypes.windll.kernel32.TerminateProcess(int(process._handle), -1)
    

def main():
    left = bonus2time()
    cw = CountdownWindow(left)
    gtk.main()
    
def bonus2time(perpoint=30, filename='bonusfile.pickle'):
    f = file(filename, 'r')
    points = pickle.load(f)
    f.close()
    
    seconds = perpoint * points
    minutes = seconds / 60
    seconds = seconds - minutes * 60
    hours = minutes / 60
    minutes = minutes - hours * 60
    return [hours, minutes, seconds]

def time2bonus(t, perpoint=30, filename='bonusfile.pickle'):
    seconds = t[0] * 60 * 60 + t[1] * 60 + t[2]
    bonus = seconds / perpoint
    
    f = file(filename, 'w')
    pickle.dump(bonus, f)
    f.close()
    

class TimeoutError(Exception):
    """A self-defined exception, raised when timeouts occur"""
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
        cmd = [progpath, sys.argv[1]]
        self.process = subprocess.Popen(cmd)
        self.terminated = False
        self.label.set_text(self.list2time(self.timeleft))
        gobject.timeout_add(1000, self.update)
    
    def update(self):
        try:
            self.decrease_time()
            self.label.set_text(self.list2time(self.timeleft))
            return True
        except TimeoutError:
            exit(self.process)
            return False
    
    def delete_event(self, widget, event):
        """Quitting the window"""
        exit(self.process)
        time2bonus(self.timeleft)
        gtk.main_quit()
        return False
    
    def list2time(self, lst):
        hours = ''
        if lst[0] < 10:
            hours += '0'
        hours += str(lst[0])
        
        minutes = ''
        if lst[1] < 10:
            minutes += '0'
        minutes += str(lst[1])
        
        seconds = ''
        if lst[2] < 10:
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
