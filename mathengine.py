#!/usr/bin/env python
# -*- encoding: latin-1 -*- 

import random, logging, pickle, time, gettext, locale, os, os.path
import gtk

__version__ = '0.2.1'
__author__ = 'Marek Kubica'
__doc__ = """A graphical, extensible program for learning elementar maths.
GUI done by GTK+ & PyGTK.
Features:
- multiple, selectable engines
- logging
- statistics
- a bonus system"""

def initlang(langoverride=None):
    if not langoverride:
        try:
            languages = os.listdir('locale')
            lang = gettext.translation('mathengine', 'locale', 
                languages=languages)
            lang.install()
        except:
            gettext.install('mathengine')
    else:
        lang = gettext.translation('mathengine', 'locale', 
            languages=[langoverride])
        lang.install()
    

class MathWindow(object):
    def __init__(self):
        """Creating the window"""
        self.create_window()
        
        self.motor = None
        self.bonus = Bonus()
        self.stat = Stats()
        logging.info(_('Program starting'))
    
    def delete_event(self, widget, event=None):
        """Quitting the window"""
        logging.info(_('Program exiting'))
        logging.info('Statistics:')
        logging.info('%d right, %d wrong of %d' % (self.stat.right, self.stat.wrong, self.stat.all))
        logging.info('That is %d%%' % self.stat.GetPercent())
        logging.info('Average time is %d seconds' % self.stat.GetAverageTime())
        
        gtk.main_quit()
        return False
    
    def create_window(self):
        # create empty window
        self.window = gtk.Window()
        # set a default size
        self.window.set_default_size(350, 100)
        # connect with the destroy event to allow closing
        self.window.connect('delete_event', self.delete_event)
        # set a title
        self.window.set_title(_('MathEngine [no engine running]'))
        
        # create a vertival box
        self.vbox = gtk.VBox()
        # add it to the window
        self.window.add(self.vbox)
        self.menubar = gtk.MenuBar()
        self.vbox.pack_start(self.menubar, expand=False)
        
        self.menu = gtk.MenuItem('Mode')
        self.mode_menu = gtk.Menu()
        self.menu.set_submenu(self.mode_menu)
        
        menuitem = gtk.MenuItem('Multiply')
        menuitem.connect('activate', self.OnMultiply)
        self.mode_menu.add(menuitem)
        self.menuitem = gtk.MenuItem('Division')
        self.menuitem.connect('activate', self.OnDivision)
        self.mode_menu.add(self.menuitem)
        self.menuitem = gtk.MenuItem('Chunkdivision')
        self.menuitem.connect('activate', self.OnChunkDivision)
        self.mode_menu.add(self.menuitem)
        
        self.menuitem = gtk.SeparatorMenuItem()
        self.mode_menu.add(self.menuitem)
        self.menuitem = gtk.MenuItem('About')
        self.menuitem.connect('activate', self.OnAbout)
        self.mode_menu.add(self.menuitem)
        self.menuitem = gtk.MenuItem('About Mode')
        self.menuitem.connect('activate', self.OnAboutMode)
        self.mode_menu.add(self.menuitem)
        self.menuitem = gtk.MenuItem('Exit')
        self.menuitem.connect('activate', self.OnExit)
        self.mode_menu.add(self.menuitem)
        
        self.menubar.append(self.menu)
        
        # Content
        self.table = gtk.Table()
        
        self.factor1 = gtk.Entry()
        self.factor1.set_sensitive(False)
        self.factor1.connect("activate", self.OnCheck)
        self.table.attach(self.factor1, 0, 1, 0, 1)
        
        self.op1 = gtk.Label()
        self.table.attach(self.op1, 1, 2, 0, 1)
        
        self.factor2 = gtk.Entry()
        self.factor2.set_sensitive(False)
        self.factor2.connect("activate", self.OnCheck)
        self.table.attach(self.factor2, 2, 3, 0, 1)
        
        self.op2 = gtk.Label()
        self.table.attach(self.op2, 3, 4, 0, 1)
        
        self.factor3 = gtk.Entry()
        self.factor3.set_sensitive(False)
        self.factor3.connect("activate", self.OnCheck)
        self.table.attach(self.factor3, 4, 5, 0, 1)
        
        self.lastresult_label = gtk.Label('Last result:')
        self.table.attach(self.lastresult_label, 0, 1, 1, 2)
        
        self.lastresult = gtk.Label('(No last result)')
        self.table.attach(self.lastresult, 2, 3, 1, 2)
        
        self.check = gtk.Button('Check it!')
        self.check.set_flags(gtk.CAN_DEFAULT)
        self.check.connect('clicked', self.OnCheck)
        self.check.set_sensitive(False)
        self.table.attach(self.check, 4, 5, 1, 2)
        
        self.vbox.pack_start(self.table)
        self.check.grab_default()
        
        self.window.show_all()
    
    def OnExit(self, widget):
        self.delete_event(widget)
    
    def OnAbout(self, widget):
        dialog = gtk.MessageDialog(self.window,
            gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
            gtk.MESSAGE_INFO, gtk.BUTTONS_OK,
            'MathEngine '  + __version__ + '\n' + 'written by ' + __author__ + '\n' + __doc__) 
        dialog.run()
        dialog.destroy() 
    
    def OnAboutMode(self, widget):
        try:
            source = self.engine
            text = source.__doc__ + '\n'+ 'Version ' + source.__version__ + '\n' + 'Status ' + source.state + '\n' + 'written by ' + source.__author__
            dialog = gtk.MessageDialog(self.window,
                gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
                gtk.MESSAGE_INFO, gtk.BUTTONS_OK,
                text)
        except AttributeError:
            dialog = gtk.MessageDialog(self.window,
                gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
                gtk.MESSAGE_ERROR, gtk.BUTTONS_OK,
                "You have chosen no engine, Sai.")
        dialog.run()
        dialog.destroy() 
    
    def OnCheck(self, widget):
        self.stat.StopTimer()
        
        if self.engine.question_field == 1:
            user_answer = int(self.factor1.get_text())
        elif self.engine.question_field == 2:
            user_answer = int(self.factor2.get_text())
        elif self.engine.question_field == 3:
            try:
                user_answer = int(self.factor3.get_text())
            except ValueError:
                user_answer = self.factor3.get_text()
        #print user_answer
        
        if self.engine.question_field == 1:
            program_answer = self.quest[0]
        elif self.engine.question_field == 2:
            program_answer = self.quest[1]
        elif self.engine.question_field == 3:
            program_answer = self.quest[2]
        #print program_answer
        
        # some magic for using colors with GTK+
        style = self.lastresult.get_style().copy()
        color = self.lastresult.get_colormap()
        
        # let's check if the answer was right
        if user_answer == program_answer:
            # the answer was right, so set text to right
            self.lastresult.set_text('Right')
            # mark if for being displayed blue
            fg = color.alloc_color('blue')
            # add the bonus
            self.bonus.add(self.engine.bonusadd)
            # add to the statistics as right
            self.stat.AddRight()
            logging.info('Right: %s %s %s %s %s' % (str(self.quest[0]),
                self.engine.ops[0], str(self.quest[1]),
                self.engine.ops[1], str(self.quest[2])))
        else:
            # no, the answer was wrong
            self.lastresult.set_text('Wrong: %s %s %s %s %s' % 
                (str(self.quest[0]), self.engine.ops[0], str(self.quest[1]), 
                self.engine.ops[1], str(self.quest[2])))
            fg = color.alloc_color('red')
            self.stat.AddWrong()
            logging.info('Wrong: %s %s %s %s %s' % (str(self.quest[0]),
                self.engine.ops[0], str(self.quest[1]),
                self.engine.ops[1], str(self.quest[2])))
        
        # now set the color
        style.fg[gtk.STATE_NORMAL] = fg
        self.lastresult.set_style(style)
        
        # go on and display the next question
        self.display()
    
    def OnMultiply(self, widget):
        """Starts the Multiply Engine"""
        self.window.set_title('MathEngine [Multiply Engine running]')
        self.motor = 'multiply'

        # call the first question
        self.display()
        # activate the button
        self.check.set_sensitive(True)
        
    def OnDivision(self, widget):
        self.window.set_title('MathEngine [Division Engine running]')
        self.motor = 'division'
        
        # call the first question
        self.display()
        # activate the button
        self.check.set_sensitive(True)
    
    def OnChunkDivision(self, widget):
        self.window.set_title('MathEngine [Chunkdivision Engine running]')
        self.motor = 'chunkdivision'
        
        # call the first question
        self.display()
        # activate the button
        self.check.set_sensitive(True)
    
    def display(self):
        if self.motor == 'multiply':
            self.engine = MultiplyEngine()
        elif self.motor == 'division':
            self.engine = DivisionEngine()
        elif self.motor == 'chunkdivision':
            self.engine = ChunkDivisionEngine()
        
        self.quest = self.engine.question()
        
        self.factor1.set_text(str(self.quest[0]))
        self.factor2.set_text(str(self.quest[1]))
        self.factor3.set_text(str(self.quest[2]))
        
        if self.engine.question_field == 1:
            self.factor1.set_sensitive(True)
            self.factor1.set_text('')
        elif self.engine.question_field == 2:
            self.factor2.set_sensitive(True)
            self.factor2.set_text('')
        elif self.engine.question_field == 3:
            self.factor3.set_sensitive(True)
            self.factor3.set_text('')
        
        self.op1.set_text(self.engine.ops[0])
        self.op2.set_text(self.engine.ops[1])
        
        self.stat.StartTimer()

class LoadableEngine:
    """A super class for all engines.
    Class members:
    __version__ - The version of the engine (the default is the version of the program)
    __author__ - The author of the engine (probably you, if you read this)
    state - the status of the engine: possible are unimplemented (default), experimental,
    unstable, stable. It has no real use, but is maybe helpful for the users
    question_field - very important! it sets the TextInputField where the user can input
    his answers (yes, this can be changed!). The default value is 3 for the third field.
    ops - a tuple (2) with the operators: the operators are strings with leading and 
    trailing spaces
    question() - outputs the question in a tuple (3), take care of question_field
    [write on]"""
    
    # set the version information equal to the programs'
    #+since is was made by the same person
    __version__ = __version__
    __author__ = __author__
    state = 'unimplemented'
    
    # which field will be checked for the answer?
    question_field = 3
    
    # which operators?
    ops = (" ? ", " ? ")
    
    # how much is a question worth?
    bonusadd = 0

class MultiplyEngine(LoadableEngine):
    """The engine for multiplying"""
    state = 'stable'
    question_field = 3
    ops = (' x ', ' = ')
    bonusadd = 1
    def question(self):
        a = random.randrange(9)
        b = random.randrange(9)
        c = a * b
        return (a, b, c)

class DivisionEngine(LoadableEngine):
    """The engine for dividing"""
    state = 'stable'
    question_field = 3
    
    ops = (" / ",  " = " )
    bonusadd = 2
    def question(self):
        b = random.randrange(9)
        b += 2
        c = random.randrange(9)
        c += 2
        a = b * c
        return (a, b, c)
        
class ChunkDivisionEngine(DivisionEngine):
    """Division engine with chunks
    watch out: if chunk == 0 you still have to
    type 'r0'"""
    state = 'stable'
    bonusadd = 5
    
    def question(self):
        # b = [1; 9]
        b = random.randrange(9)
        b += 1
        
        # c = [1; 9]
        c = random.randrange(9)
        c += 1
        
        if b != 1:
            chunk = random.randrange(b - 1)
            chunk += 1
        else:
            chunk = 0
        
        a = b * c + chunk
        cnew = str(c) + "r" + str(chunk)
        return (a, b, cnew)
        
class Stats(object):
    """A class for statistics"""
    
    def __init__(self):
        self.all = 0
        self.right = 0
        self.wrong = 0
        self.time = 0
        self.times = []
    
    def AddRight(self):
        """One right"""
        self.right += 1
        self.all += 1
    
    def AddWrong(self):
        """One wrong"""
        self.wrong += 1
        self.all += 1
    
    def GetPercent(self):
        """Get the percent of right responses"""
        if self.all != 0:
            percent = int((float(self.right) / float(self.all)) * 100)
        else:
            percent = 0
        return percent
        
    def StartTimer(self):
        """Starts the timer"""
        self.time = time.time()
    
    def StopTimer(self):
        """Stops the timer and adds the delta to the times db"""
        time_stop = time.time()
        time_delta = time_stop - self.time
        # reset the timer
        self.time = 0
        
        # get just the seconds...
        time_delta = int(time_delta)
        # append the value to the statistic
        self.times.append(time_delta)
    
    def GetAverageTime(self):
        """Returns the average time of all archived times"""
        values = len(self.times)
        if values != 0:
            alltime = 0
            for time in self.times:
                alltime += time
            average = alltime / values
            return average
        else:
            return 0

def initlog(filename='mathengine.log'):
    """Use later with logging.info()"""
    logging.basicConfig(level=logging.DEBUG,
        format='%(asctime)s %(message)s',
        datefmt='%Y%m%d %H:%M:%S',
        filename=filename)

class Bonus(object):
    def __init__(self, filename='bonusfile.pickle'):
        self.bonusfile = filename
        try:
            f = file(self.bonusfile, 'r')
            self.bonus = pickle.load(f)
            f.close()
        except IOError:
            self.bonus = 0
    
    def add(self, points):
        self.bonus += points
        self.save()
    
    def save(self):
        f = file(self.bonusfile, 'w')
        pickle.dump(self.bonus, f)
        f.close()

def main():
    initlang()
    initlog()
    mw = MathWindow()
    gtk.main()
    

if __name__ == '__main__':
    main()