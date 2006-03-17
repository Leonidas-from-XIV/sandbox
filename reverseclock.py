#!/usr/bin/env python
# -*- encoding: latin-1 -*- 

import datetime, math
import pygame
import pygame.locals as pyl

class ReverseClock(object):
    """A reversed clock
    digitime - set True to display a small digital clock
    trueclock -  set True to make the clock go properly
    smooth - digital or smooth pointer movement, increase fps for smooth
    fps - frames per second rendered"""
    
    def __init__(self, side):
        """Initializes the window, some variables and that stuff"""
        # initialise pygame
        pygame.init()
        self.screen = pygame.display.set_mode((side, side))
        pygame.display.set_caption('Reversed Clock')
        
        self.background = pygame.Surface(self.screen.get_size())
        self.background = self.background.convert()
        
        self.center = side / 2
        
        # also display the digital clock? Useful for debugging
        self.digitime = False
        # the font to display the time
        self.font = pygame.font.Font(None, 18)
        
        # work like a normal clock?
        self.trueclock = False
        
        # smooth movement or rather digital
        self.smooth = False
        self.fps = 5
        
        # also known as #FFOOFF
        # foreground color
        self.color = (255, 0, 255)
        # background color
        self.back_color = (0, 0, 0)
        
        # create a clock to control the FPS
        self.clock = pygame.time.Clock()
    
    def tick(self):
        """Draws the clock"""
        
        # blank the screen: set it black
        self.background.fill(self.back_color)
        # draw a circle
        pygame.draw.circle(self.background, self.color, 
            (self.center, self.center), self.center, self.center / 66)
        
        # get the current time
        current = datetime.datetime.today()
        
        if current.hour > 13:
            current_hour = current.hour - 12
        
        # the first element is the current value,
        # the second element is the number of pieces
        # the third one it the length of the pointer
        if self.smooth:
            parts = {
                'second' : (current.second + current.microsecond/1000000.0, 60, 1 * self.center),
                'minute' : (current.minute + current.second/60.0, 60, 0.8 * self.center),
                'hour' : (current_hour + current.minute/60.0, 12, 0.6 * self.center)
                }
        else:
            parts = {
                'second' : (current.second, 60, 1 * self.center),
                'minute' : (current.minute, 60, 0.8 * self.center),
                'hour' : (current_hour, 12, 0.6 * self.center)
                }
        results = {}
        
        for part, value in parts.items():
            piece, divide, length = value
            
            # set the clock mode
            if not self.trueclock:
                # piece = -piece means: reversed clock
                piece = -piece
            
            angle = (360.0 / divide) * piece
            angle -= 90
            x_coord = math.cos(math.radians(angle)) * length
            y_coord = math.sin(math.radians(angle)) * length
            
            destination = (self.center + x_coord, self.center + y_coord)
            results[part] = destination
        
        # draw the pointers
        pygame.draw.line(self.background, self.color, (self.center, self.center), results['second'], self.center / 200)
        pygame.draw.line(self.background, self.color, (self.center, self.center), results['minute'], self.center / 66)
        pygame.draw.line(self.background, self.color, (self.center, self.center), results['hour'], self.center / 40)
        
        # blit the pointers on the screen
        self.screen.blit(self.background, (0, 0))
        
        if self.digitime:
            # the time in 12h:minute:second format
            timedisplay = current.strftime('%I:%M:%S')
            # the surface with the text: black text on white background
            display = self.font.render(timedisplay, True, (0, 0, 0), (255, 255, 255))
            self.screen.blit(display, (0, 0))
        
        # flip the display-screen
        pygame.display.flip()
        
    def main_loop(self):
        """The main loop.
        Calls tick() until the end"""
        while True:
            # limit FPS
            self.clock.tick(self.fps)
            
            # catch events
            for event in pygame.event.get():
                if event.type == pyl.QUIT:
                    return
                elif event.type == pyl.KEYDOWN and event.key == pyl.K_d:
                    # digitime switch
                    self.digitime = not self.digitime
                elif event.type == pyl.KEYDOWN and event.key == pyl.K_r:
                    # reverse switch
                    self.trueclock = not self.trueclock
                elif event.type == pyl.KEYDOWN and event.key == pyl.K_s:
                    # smooth switch
                    self.smooth = not self.smooth
            
            # draw the clock
            self.tick()

def main():
    # get an instance
    rc = ReverseClock(400)
    #rc.digitime = True
    #rc.trueclock = True
    #rc.smooth = True
    #rc.color = (255, 0, 0)
    # let it run forever
    rc.main_loop()

if __name__ == '__main__':
    main()