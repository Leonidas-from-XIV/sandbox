#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import ctypes
import win32api, win32process, win32con, win32gui
import gtk, gobject, pango

def hibernate():
    """Hibernates the computer"""
    # a handle to PowrProf.dll
    pwr = ctypes.cdll.PowrProf
    # hibernate
    pwr.SetSuspendState(True)
    
class PlayerController(object):
    def __init__(self):
        # a starting value for the pid
        self.pid = None
    
    def _enum(self, hwnd, dummy):
        """Do not call this by yourself - it wouldn't work.
        Finds the PID of the foobar2000 process"""
        
        text = win32gui.GetWindowText(hwnd)
        if "foobar2000" in text :
            #print text, hwnd
            tid, pid = win32process.GetWindowThreadProcessId(hwnd)
            # got our pid!
            self.pid = pid
    
    def quit(self):
        win32gui.EnumWindows(self._enum, None)
        #print self.pid
        if self.pid:
            # get a handle
            handle = win32api.OpenProcess(win32con.PROCESS_TERMINATE, 0, self.pid)
            # terminate the process by the handle
            win32api.TerminateProcess(handle, 0)
            # close the handle - thankyou
            win32api.CloseHandle(handle)

class CountdownWindow(object):
    def __init__(self):
        self.window = gtk.Window()
        # connect with the destroy event to allow closing
        self.window.connect('delete_event', gtk.main_quit)
        # set a title
        self.window.set_title('Sleeplayer')
        
        # create a label tor the text
        self.label = gtk.Label('00:00:00')
        # display that label rather big
        self.label.modify_font(pango.FontDescription('30'))
        
        # create a vertical box
        vsizer = gtk.VBox()
        # add the sizer to that box
        vsizer.add(self.label)
        
        # create a horizontal box
        hsizer = gtk.HBox()
        # add the horizontal box to the vertical
        vsizer.add(hsizer)
        # a list of buttons
        times = [1, 30, 60, 90]
        
        self.timecombo = gtk.combo_box_new_text()
        self.timecombo.connect('changed', self.changed_time)
        
        for minutes in times:
            self.timecombo.append_text(str(minutes))
        hsizer.add(self.timecombo)
        
        self.modecombo = gtk.combo_box_new_text()
        
        modes = ['Exit player', 'Hibernate']
        for mode in modes:
            self.modecombo.append_text(mode)
        #print dir(self.modecombo)
        self.modecombo.set_active(0)
        hsizer.add(self.modecombo)    
        
        # add the vertical box to the window
        self.window.add(vsizer)
        # show all widgets
        self.window.show_all()
        
        # initial time
        self.timeleft = 0
        # set the text for the label
        self.label.set_text(self.display_time())
        
        self.time_id = None
    
    def changed_time(self, widget):
        mins = int(widget.get_active_text())
        self.set_minutes(mins)
    
    def set_minutes(self, mins):
        """Sets the selected minutes"""
        
        # convert the selected minutes to seconds
        self.timeleft = mins * 60
        # set the time
        self.label.set_text(self.display_time())
        
        if self.time_id:
            gobject.source_remove(self.time_id)
        self.time_id = gobject.timeout_add(1000, self.tick)
    
    def act(self):
        # quitting GTK+
        gtk.main_quit()
        
        # shutting down player
        pc = PlayerController()
        pc.quit()
        
        if self.modecombo.get_active_text() == 'Hibernate':
            hibernate()
    
    def tick(self):
        """Ticks every second when running. This is called
        automatically. Counts down the time and displays it."""
        # decrease the remaining time
        self.timeleft -= 1
        
        if self.timeleft > 0:
            # display the time
            self.label.set_text(self.display_time())
            # let the timeout run on
            return True
        else:
            # point zero reached - acting
            self.act()
            # canceling timeout function
            return False
    
    def display_time(self):
        """Converts the seconds left into HH:MM:SS formatted
        strings ready for displaying"""
        # get the hours (seconds is just temporary)
        hours, seconds = divmod(self.timeleft, 60*60)
        # get the minutes and seconds now
        minutes, seconds = divmod(seconds, 60)
        
        # format them into a list
        numbers = [hours, minutes, seconds]
        numbers_ready = []
        
        # go through this list and add zeros where the number is < 10
        for number in numbers:
            if number < 10:
                numbers_ready.append('0' + str(number))
            else:
                numbers_ready.append(str(number))
        
        # return the concatenated string
        return ':'.join(numbers_ready)

def main():
    #PlayerController().quit()
    cw = CountdownWindow()
    gtk.main()

if __name__ == '__main__':
    main()
