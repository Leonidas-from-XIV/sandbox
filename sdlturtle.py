#!/usr/bin/env python
# -*- encoding: latin-1 -*- 

import math, threading, time
import pygame
import pygame.locals as pyl

class PlainTurtle(object):
    """The Turtle class"""
    # the width of the window
    screenwidth = 680
    # the height of the window
    screenheight = 480
    # a tuple of both sizes
    screensize = (screenwidth, screenheight)
    # window to clamp the trutle into
    # not yet used
    clamp_window = pyl.Rect(0, 0, screenwidth, screenheight)
    # number of frames per second to limit to
    fps = 10
    
    def __init__(self):
        """Initialize the turtle:
        set a default position and a default direction"""
        # initialise pygame
        pygame.init()
        self.screen = pygame.display.set_mode(self.screensize)
        self.background = pygame.Surface(self.screen.get_size())
        self.background.fill((255, 255, 255))
        self.clock = pygame.time.Clock()
        #self.display()
        
        self.position = (100, 100)
        # maybe encapsulate direction with a property 
        # to make it 0 <= direction <) 2* math.pi
        self.direction = math.radians(45)
    
    def mainloop(self):
        """A mainloop. Just displays the contents
        of the surfaces and waits"""
        while True:
            self.clock.tick(self.fps)
            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT:
                    # then the user closed the window
                    return
                    # maybe better raise an exception here
    
    def forward(self, distance):
        """Move the Turtle forward"""
        
        end_position = ()
        # check whether the distance is negative: i.e. go backwards.
        negative = distance < 0
        if negative:
            distance = -distance
        
        # prepare a copy of the background surface
        backcopy = None
        
        for scale in range(1, distance + 1):
            # limit speed to fps
            self.clock.tick(self.fps)
            
            # create a copy of the background surface
            backcopy = self.background.copy()
            
            if negative:
                # moving backwards
                scale = - scale
            
            # calculate the coordinate offsets
            x_offset = scale * math.cos(self.direction)
            y_offset = - scale * math.sin(self.direction)
            #print x_offset, y_offset
            # calculate the end position of the turtle
            end_position = (self.position[0] + x_offset, self.position[1] + y_offset)
            
            # draw the trail of the turtle
            pygame.draw.line(backcopy, (0, 0, 0), self.position, end_position)
            
            self.screen.blit(backcopy, (0, 0))
            pygame.display.flip()
        
        # set the final position
        self.position = end_position
        # copy the surface back
        self.background = backcopy
    
    def backward(self, distance):
        self.forward(-distance)
        
    def left(self, angle):
        self.direction += math.radians(angle)
    
    def right(self, angle):
        self.direction -= math.radians(angle)

class Turtle(threading.Thread):
    """PlainTurtle wrapped into a thread"""
    def __init__(self):
        """Thread constructor"""
        threading.Thread.__init__(self)
        # no, we're not yet initialized
        self.ready = False
        # start thread initialisation in.. thread
        self.start()
        
        # wait until self.ready signalizes it is ready
        while not self.ready:
            #print 'waiting', self.ready
            time.sleep(0.1)
    
    def run(self):
        """This gets called automatically by
        the constructor while starting
        the thread"""
        self.turtle = PlainTurtle()
        
        # add names from turtle to TurtleThread class
        for name in dir(self.turtle):
            self.__dict__[name] = getattr(self.turtle, name)
        
        # now the initialisation is ready
        self.ready = True
        # just go inside the mainloop and stay there
        self.turtle.mainloop()

def main():
    """A small demonstration of turtles possibilities.
    Also useful for testing"""
    t = Turtle()
    t.forward(10)
    t.left(90)
    t.forward(10)
    t.right(90)
    t.forward(10)

if __name__ == '__main__':
    main()
