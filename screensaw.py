#!/usr/bin/env python
"""A program for demonstrating the graphical abilities of Python.
It uses just the 2D mode, who needs 3D?
Some demos are taken from XScreenSaver (really cool prog)
and simply ported to Python (without looking in the code)

Requirements:
- Python (written with 2.4)
- pygame (written with 1.6)

Written by Marek Kubica.
Dedicated to Fritz Cizmarov
"""
import os, sys, random, time, math
import pygame
import pygame.locals as pyl

__version__ = '0.0.7'


screenwidth = 680
screenheight = 480
screensize = (screenwidth, screenheight)
fps = 60

# Does the demos die on small problems?
deathtrap = False
# verbose output?
verbose = False

def prepare():
    """Internal: Prepare the system for running demos"""
    # Initialise pygame
    pygame.init()
    
    # open a window
    global screen
    screen = pygame.display.set_mode(screensize)
    # set the window title
    pygame.display.set_caption('Screensaw ' + __version__)
    # hide mouse
    pygame.mouse.set_visible(0)
    
    # create the surface
    global background
    background = pygame.Surface(screen.get_size())
    # convert it to be faster
    background = background.convert()
    #background = background.convert_alpha()
    # fill it black
    background.fill((0, 0, 0))
    background.set_alpha(100)
    # create a clock to control the FPS
    global clock
    clock = pygame.time.Clock()
    

def main():
    """Starts the demos""" 
    # We have to prepare display
    prepare()
    
    # run the demos
    #info("Screensaw " + __version__, "The Python GFX demonstration program")
    #reblank(frameskip=8)
    #info("Critter", "The first demo, a very simple one")
    #critter()
    #reblank(frameskip=8)
    #info("Ant", "A more interessting demo, ressembles Snake")
    #ant()
    #reblank(frameskip=8)
    #info("Popsquares", "The XScreenSaver Popsquares ported to Python")
    #popsquares(slow=5)
    #reblank(frameskip=8)
    #magnets()
    #reblank(frameskip=8)
    
    axe_alaska()
    
def display():
    """Displays the stuff"""
    screen.blit(background, (0, 0))
    pygame.display.flip()
    
def reblank(frameskip=4):
    """Blanks the screen
    Can make the animation faster or slower by specifying frameskip"""
    colors = 256 / frameskip
    grays = range(colors)
    
    # black to white
    for color in grays:
        clock.tick(fps)
        color = color * frameskip
        background.fill((color, color, color))
        display()
        
    grays.reverse()
    
    # white to black
    for color in grays:
        clock.tick(fps)
        color = color * frameskip
        background.fill((color, color, color))
        display()

def info(caption, description, slow=2):
    """Shows the description of a demo"""
    
    #len of the whole text
    lenght = len(caption) + len(description)
    
    # frames to end
    end = lenght * slow
    # current frame
    pointer = 0
    
    while pointer < end:
        clock.tick(fps)
        
        # handle events
        for event in pygame.event.get():
            if event.type == pyl.QUIT:
                return
            elif event.type == pyl.KEYDOWN:
                # quit on keyboard
                return
        
        # creates the caption font
        caption_font = pygame.font.Font(None, 50)
        caption_size = caption_font.size(caption)
        cpt = caption_font.render(caption, 1, (255, 255, 255))
    
        # creates the description font
        description_font = pygame.font.Font(None, 22)
        description_size = description_font.size(description)
        dsc = description_font.render(description, 0, (255, 255, 255))
    
        # shows
        #screen.blit(cpt, (caption_size[0] -100 , 40 + caption_size[1]))
        screen.blit(cpt, (75 , 100))
    
        #screen.blit(dsc, (description_size[0] + 10, 40 + description_size[1]))
        #screen.blit(dsc, (caption_size[0] -100, description_size[1] + 115))
        screen.blit(dsc, (75, 155))
        pygame.display.flip()
        
        # display just a short time
        pointer += 1
        
    
    # replace
    #time.sleep(2)
    
    
    

def popsquares(slow=20):
    """POPSquares demo - like this one of XScreenSaver
    It can be made faster or slower by setting slow.
    The higher slow is, the slower the demo.
    You should use values between 1 and 60 or let the default: 20."""
    squaresheight = screenheight / 5
    squareswidth = screenwidth / 5
    
    # test the input
    if slow == 0:
        raise ZeroDivisionError("You should take values higher than zero, Sai.")
    
    # we have to prepare the initial popsquares
    prepared = False
    while not prepared:
        # the squares in a line
        for yposition in xrange(screenheight / squaresheight):
            ycoords = yposition * squaresheight
            
            # the positions of the squares in the line
            for xposition in xrange(screenwidth / squareswidth):
                bluecolor = random.randrange(250)
                xcoord = xposition * squareswidth
                re = pygame.Rect((xcoord, ycoords), (squareswidth, squaresheight))
                pygame.draw.rect(background, (0, 0, bluecolor), re, 0)
        
        # create the lines
        poplines((squareswidth, squaresheight))
        # we're done with preparation
        prepared = True
    
    # start animation
    while True:
        # limit fps - here's the speed defined
        clock.tick(fps / slow)
        
        #the color of the square
        bluecolor = random.randrange(250)
        
        # which square in line
        xposition = random.randrange(screenwidth / squareswidth)
        # which square in row
        yposition = random.randrange(screenheight / squaresheight)

        # handle events
        for event in pygame.event.get():
            if event.type == pyl.QUIT:
                return
            elif event.type == pyl.KEYDOWN:
                # quit on keyboard
                return
    
        # calculate the coords of the squares
        xcoord = xposition * squareswidth
        ycoord = yposition * squaresheight
        

        re = pygame.Rect((xcoord, ycoord), (squareswidth, squaresheight))
        pygame.draw.rect(background, (0, 0, bluecolor), re, 0)
        
        # redraw the lines (because the lines are overlaped by the new squares)
        poplines((squareswidth, squaresheight))
        
        display()
        
def poplines(squaressize):
    """This is not a demo - this is a helper to popsquares
    It displays the lines between the squares.
    Usage:
        poplines((width_of_square, height_of_square))
    """
    squareswidth = squaressize[0]
    squaresheight = squaressize[1]
    
    # the color of the lines - blue
    linecolor = (0, 0, 255)
    
    # display the vertical lines
    for line in xrange((screenwidth / squareswidth) -1):
        line += 1
        startX = line * squareswidth
        pygame.draw.line(background, linecolor, (startX, 0), (startX, screenheight))
    # display the horizontal lines
    for line in xrange((screenheight / squaresheight) -1):
        line += 1
        startY = line * squaresheight
        pygame.draw.line(background, linecolor, (0, startY), (screenwidth, startY))
    display()

def magnets():
    """Displays some magnets"""
    squaresize = (150, 40)
    squarecolor = (255, 0, 255)
    mass_upper_magnet = 2
    mass_lower_magnet = mass_upper_magnet
    # calculate the position of the fixed magnet
    square_lower_position = ((screenwidth / 2 - squaresize[0] /2), (screenheight - squaresize[1]))
    # calculate the position of the upper magnet (the movable one)
    square_upper_position = (0, 0)
    #print square_lower_position
    
    # The G Constant
    G = 6.67 * 10-11
    # what is 10-11?
    
    magnet_lower = pygame.Rect(square_lower_position, squaresize)
    magnet_upper = pygame.Rect(square_upper_position, squaresize)
    #print magnet_lower
    print 'Current position of upper magnet: ', square_upper_position
    pygame.draw.rect(background, squarecolor, magnet_lower)
    pygame.draw.rect(background, squarecolor, magnet_upper)
    
    while True:
        for event in pygame.event.get():
            if event.type == pyl.QUIT:
                return
            elif event.type == pyl.KEYDOWN:
                # quit on keyboard
                return
        display()
    
def laser():
    """Laser demo (unfinished) - taken from XScreenSaver"""
    pass
    
    
def ant():
    """This demo shows a kind of snake"""
    global lastcoords
    # Where should the snake start?
    lastcoords = [100, 100]
    # how big should a snakesquare be?
    squaresize = 10
    
    while True:
        # limit to 60 fps
        clock.tick(fps)
        direction = random.randrange(4)
        #0 down, 1 up, 2 left, 3 right
        
        # to hardcode direction
        #direction = 2

        # handle events
        for event in pygame.event.get():
            if event.type == pyl.QUIT:
                return
            elif event.type == pyl.KEYDOWN:
                # quit on keyboard
                return
    
        
        re = pygame.Rect(lastcoords, (10, 10))
        
        if direction == 0:
            # 0 -down
            if lastcoords[1] <= (screenheight - 10):
                lastcoords[1] = lastcoords[1] + 10
            else:
                if verbose:
                    print "L[1]", lastcoords[1], "Too down!"
                if deathtrap:
                    sys.exit(9)
                else:
                    pass
                
                
        elif direction == 1:
            # 1 -up
            if lastcoords[1] >= 10:
                lastcoords[1] = lastcoords[1] - 10
            else:
                if verbose:
                    print "L[1]", lastcoords[1], "Too up!"
                if deathtrap:
                    sys.exit(9)
                else:
                    pass
                
        elif direction == 2:
            # 2 - left
            if lastcoords[0] >= 10:
                lastcoords[0] = lastcoords[0] - 10
            else:
                #pass
                if verbose:
                    print "L[0]", lastcoords[0], "Too left!"
                if deathtrap:
                    sys.exit(9)
                else:
                    pass
                
        elif direction == 3:
            # 3 -right 
            if lastcoords[0] <= screenwidth:
                lastcoords[0] = lastcoords[0] + 10
            else:
                if verbose:
                    print "L[0]", lastcoords[0], "Too right!"
                if deathtrap:
                    sys.exit(9)
                else:
                    pass
        
        pygame.draw.rect(background, (255, 255, 255), re, 0)
        display()
    del(lastcoords)

def critter():
    """Make some crittering on the screen"""

    while True:
        # limit to 60 fps
        clock.tick(fps)

        # handle events
        for event in pygame.event.get():
            if event.type == pyl.QUIT:
                return
            elif event.type == pyl.KEYDOWN:
                # quit on keyboard
                return
    
        # draw a line
        startX = random.randrange(screenwidth)
        startY = random.randrange(screenheight)
        endX = random.randrange(screenwidth)
        endY = random.randrange(screenheight)
        pygame.draw.line(background, (255, 255, 255), (startX, startY), (endX, endY))
        
        display()

def axe_alaska():
    """Make that AXE Alaska Logo"""

    #coords = create_hexagon((100, 200), 50)
    # first x value, then y value
    #coords = create_hexagon((50, 43), 50)
    #hexface = pygame.Surface(hexagon_size(coords))
    
    #yellow = (251, 224, 29)
    #white = (255, 255, 255)
    #blue = (46, 144, 189)
    
    #pygame.draw.polygon(hexface, yellow, coords, 0)
    #hexface = pygame.transform.rotate(hexface, 90)
    
    #hexface.convert_alpha()
    #hexface.set_alpha(50)
    #screen.blit(hexface, (10, 50))
    #pygame.display.flip()
    
    hs = HexaSprite()
    screen.blit(hs.surface, (10, 100))
    #pygame.display.flip()
    pygame.display.update()
    #print dir(hs)
    
    while True:
        # limit to 60 fps
        clock.tick(fps)

        # handle events
        for event in pygame.event.get():
            if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                return
                
        #hexface = pygame.transform.rotate(hexface, 1)
        screen.fill((0, 0, 0))
        hs.move_right()
        screen.blit(hs.surface, hs.rect)
        pygame.display.flip()
        #pygame.display.update()

def create_hexagon(center, radius):
    center_x = center[0]
    center_y = center[1]
    
    middleleft = (center_x - radius, center_y)
    middleright = (center_x + radius, center_y)
    
    d = math.sqrt(radius ** 2 - (radius/2.0) ** 2)
    deviation = int(round(d))
    
    upperleft = (int(round(center_x - radius/2.0)), center_y - deviation)
    upperright = (int(round(center_x + radius/2.0)), center_y - deviation)
    lowerleft = (int(round(center_x - radius/2.0)), center_y + deviation)
    lowerright = (int(round(center_x + radius/2.0)), center_y + deviation)
    
    return (upperleft, upperright, middleright, lowerright, lowerleft, middleleft)

def hexagon_size(hexagon):
    left = hexagon[5][0]
    right = hexagon[2][0]
    width = right - left
    
    top = hexagon[0][1]
    buttom = hexagon[3][1]
    height = buttom - top
    
    return (width, height)

class HexaSprite(pygame.sprite.Sprite):
    def __init__(self):
        pygame.sprite.Sprite.__init__(self)
        
        yellow = (251, 224, 29)
        coords = create_hexagon((50, 43), 50)
        self.surface = pygame.Surface(hexagon_size(coords))
        self.rect = self.surface.get_rect()
        
        pygame.draw.polygon(self.surface, yellow, coords, 0)
        self.surface.convert_alpha()
        self.surface.set_alpha(50)
    
    def move_up(self):
        if self.rect.top > 0:
            self.rect.top -= 1
            return True
        else:
            return False
    
    def move_down(self):
        if self.rect.bottom < screenheight:
            self.rect.top += 1
            return True
        else:
            return False
        
    def move_right(self):
        if self.rect.right < screenwidth:
            self.rect.left += 1
            return True
        else:
            return False
    
    def move_left(self):
        if self.rect.left > 0:
            self.rect.left -= 1
            return True
        else:
            return False
        
if __name__ == '__main__':
    main()