#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A program for demonstrating the graphical abilities of Python.
It uses just the 2D mode, who needs 3D?
Some demos are taken from XScreenSaver (really cool prog)
and simply ported to Python (without looking in the code)

Requirements:
- Python (written with 2.4)
- pygame (written with 1.6)

Written by Marek Kubica.
Dedicated to Fritz Cizmarov.
"""
import os, sys, random, time, math, optparse
import pygame
import pygame.locals as pyl

__version__ = '0.1.7'

class Application(object):
    screenwidth = 680
    screenheight = 480
    screensize = (screenwidth, screenheight)
    clamp_window = pyl.Rect(0, 0, screenwidth, screenheight)
    fps = 60
    
    def __init__(self):
        """Internal: Prepare the system for running demos"""
        # initialise pygame
        pygame.init()
    
        # open a window
        self.screen = pygame.display.set_mode(self.screensize)
        # set the window title
        pygame.display.set_caption('Screensaw ' + __version__)
        # hide mouse
        pygame.mouse.set_visible(False)
    
        # create the surface
        self.background = pygame.Surface(self.screen.get_size())
        # convert it to be faster
        self.background = self.background.convert()
        # fill it black
        self.background.fill((0, 0, 0))
        # create a clock to control the FPS
        self.clock = pygame.time.Clock()
    
    def critter(self):
        """Make some crittering on the screen"""

        while True:
            # limit to 60 fps
            self.clock.tick(self.fps)

            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT:
                    return
                elif event.type == pyl.KEYDOWN:
                    # quit on keyboard
                    return
    
            # draw a line
            startX = random.randrange(self.screenwidth)
            startY = random.randrange(self.screenheight)
            endX = random.randrange(self.screenwidth)
            endY = random.randrange(self.screenheight)
            pygame.draw.line(self.background, (255, 255, 255), (startX, startY), (endX, endY))
        
            self.display()
    
    def display(self):
        """Displays the stuff"""
        self.screen.blit(self.background, (0, 0))
        pygame.display.flip()
        
    def reblank(self, frameskip=4):
        """Blanks the screen
        Can make the animation faster or slower by specifying frameskip"""
        colors = 256 / frameskip
        grays = range(colors)
    
        # black to white
        for color in grays:
            self.clock.tick(self.fps)
            color = color * frameskip
            self.background.fill((color, color, color))
            self.display()
        
        grays.reverse()
    
        # white to black
        for color in grays:
            self.clock.tick(self.fps)
            color = color * frameskip
            self.background.fill((color, color, color))
            self.display()
        
    def info(self, caption, description, slow=2):
        """Shows the description of a demo"""
    
        #len of the whole text
        lenght = len(caption) + len(description)
    
        # frames to end
        end = lenght * slow
        # current frame
        pointer = 0
    
        while pointer < end:
            self.clock.tick(self.fps)
        
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
            self.screen.blit(cpt, (75 , 100))
    
            #screen.blit(dsc, (description_size[0] + 10, 40 + description_size[1]))
            #screen.blit(dsc, (caption_size[0] -100, description_size[1] + 115))
            self.screen.blit(dsc, (75, 155))
            pygame.display.flip()
        
            # display just a short time
            pointer += 1
    
    def ant(self):
        """This demo shows a kind of snake"""
        # Where should the snake start?
        lastcoords = [100, 100]
        # how big should a snakesquare be?
        squaresize = 10
    
        while True:
            # limit to 60 fps
            self.clock.tick(self.fps)
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
                if lastcoords[1] <= (self.screenheight - 10):
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
                if lastcoords[0] <= self.screenwidth:
                    lastcoords[0] = lastcoords[0] + 10
                else:
                    if verbose:
                        print "L[0]", lastcoords[0], "Too right!"
                    if deathtrap:
                        sys.exit(9)
                    else:
                        pass
        
            pygame.draw.rect(self.background, (255, 255, 255), re, 0)
            self.display()
        del(lastcoords)
    
    def popsquares(self, slow=20):
        """POPSquares demo - like this one of XScreenSaver
        It can be made faster or slower by setting slow.
        The higher slow is, the slower the demo.
        You should use values between 1 and 60 or let the default: 20."""
        
        def poplines(squaressize):
            """This is not a demo - this is a helper to popsquares
            It displays the lines between the squares.
            Usage:
                poplines((width_of_square, height_of_square))
    
            Move this into def popsquares"""
            squareswidth = squaressize[0]
            squaresheight = squaressize[1]
    
            # the color of the lines - blue
            linecolor = (0, 0, 255)
    
            # display the vertical lines
            for line in xrange((self.screenwidth / squareswidth) -1):
                line += 1
                startX = line * squareswidth
                pygame.draw.line(self.background, linecolor, (startX, 0), (startX, self.screenheight))
            # display the horizontal lines
            for line in xrange((self.screenheight / squaresheight) -1):
                line += 1
                startY = line * squaresheight
                pygame.draw.line(self.background, linecolor, (0, startY), (self.screenwidth, startY))
            self.display()

        
        squaresheight = self.screenheight / 5
        squareswidth = self.screenwidth / 5
    
        # test the input
        if slow == 0:
            raise ZeroDivisionError("You should take values higher than zero, Sai.")
    
        # we have to prepare the initial popsquares
        prepared = False
        while not prepared:
            # the squares in a line
            for yposition in xrange(self.screenheight / squaresheight):
                ycoords = yposition * squaresheight
            
                # the positions of the squares in the line
                for xposition in xrange(self.screenwidth / squareswidth):
                    bluecolor = random.randrange(250)
                    xcoord = xposition * squareswidth
                    re = pygame.Rect((xcoord, ycoords), (squareswidth, squaresheight))
                    pygame.draw.rect(self.background, (0, 0, bluecolor), re, 0)
        
            # create the lines
            poplines((squareswidth, squaresheight))
            # we're done with preparation
            prepared = True
    
        # start animation
        while True:
            # limit fps - here's the speed defined
            self.clock.tick(self.fps / slow)
        
            #the color of the square
            bluecolor = random.randrange(250)
        
            # which square in line
            xposition = random.randrange(self.screenwidth / squareswidth)
            # which square in row
            yposition = random.randrange(self.screenheight / squaresheight)

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
            pygame.draw.rect(self.background, (0, 0, bluecolor), re, 0)
        
            # redraw the lines (because the lines are overlaped by the new squares)
            poplines((squareswidth, squaresheight))
        
            self.display()
    
    def magnets(self):
        """Displays some magnets"""
        squaresize = (150, 40)
        squarecolor = (255, 0, 255)
        mass_upper_magnet = 2
        mass_lower_magnet = mass_upper_magnet
        # calculate the position of the fixed magnet
        square_lower_position = ((self.screenwidth / 2 - squaresize[0] /2), (self.screenheight - squaresize[1]))
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
        pygame.draw.rect(self.background, squarecolor, magnet_lower)
        pygame.draw.rect(self.background, squarecolor, magnet_upper)
    
        while True:
            for event in pygame.event.get():
                if event.type == pyl.QUIT:
                    return
                elif event.type == pyl.KEYDOWN:
                    # quit on keyboard
                    return
            self.display()
    
    def laser(self, speed=5, noblank=False):
        """Laser demo - taken from XScreenSaver"""
        def nextpos(speed):
            target = [0, 0]
            direction = 'right'
            while True:
                if direction == 'right':
                    if target[0] < self.screenwidth:
                        target[0] += speed
                        yield target
                    else:
                        direction = 'down'
                
                if direction == 'down':
                    if target[1] < self.screenheight:
                        target[1] += speed
                        yield target
                    else:
                        direction = 'left'
                    
                if direction == 'left':
                    if target[0] > 0:
                        target[0] -= speed
                        yield target
                    else:
                        direction = 'up'
                
                if direction == 'up':
                    if target[1] > 0:
                        target[1] -= speed
                        yield target
                    else:
                        direction = 'right'
    
        laserposition = (200, 300)
        laserlight = (255, 0, 0)
    
        pos = nextpos(speed)
    
        while True:
            # limit fps
            self.clock.tick(self.fps)

            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                    return
        
            if not noblank:
                screen.fill((0, 0, 0))
        
            target = pos.next()
                
            pygame.draw.line(self.screen, laserlight, laserposition, target, 1)
            pygame.display.update()

    def multilaser(self, speed=5):
        """Laser demo - taken from XScreenSaver"""
    
        laserposition = (200, 300)
        laserlight = (255, 0, 0)
        rays = 25
        endcoords = []
    
        pos = self.laser_nextpos(speed)
        for i in range(rays):
            c = pos.next()
            endcoords.append([c[0], c[1]])
        #raise NotImplementedError
    
        while True:
            # limit fps
            self.clock.tick(self.fps)

            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                    return
        
            self.screen.fill((0, 0, 0))
        
            for end in endcoords:
                pygame.draw.line(self.screen, laserlight, laserposition, end, 1)
            #pygame.draw.line(screen, laserlight, laserposition, target, 1)
        
            nexttarget = pos.next()
            endcoords.pop(0)
            endcoords.append([nexttarget[0], nexttarget[1]])
        
            pygame.display.update()

    def laser_nextpos(self, speed=1):
        """Move into def laser?"""
        target = [0, 0]
        direction = 'right'
        while True:
            if direction == 'right':
                if target[0] < self.screenwidth:
                    target[0] += speed
                    yield target
                else:
                    direction = 'down'
                
            if direction == 'down':
                if target[1] < self.screenheight:
                    target[1] += speed
                    yield target
                else:
                    direction = 'left'
                    
            if direction == 'left':
                if target[0] > 0:
                    target[0] -= speed
                    yield target
                else:
                    direction = 'up'
                
            if direction == 'up':
                if target[1] > 0:
                    target[1] -= speed
                    yield target
                else:
                    direction = 'right'
        
    def sinwave(self, colorchange=False, thick=False):
        """Draws a sine wave"""
        # set some starting values
        degree, linepos = 0, 0
    
        # the color of the points drawn.. can be even changed to archive cool
        # color effects like a color changing wave
        color = [255, 255, 255]
        # white now
        startpos, endpos = (0, self.screenheight / 2), (self.screenwidth, self.screenheight / 2)
    
        # draw the actual line
        pygame.draw.line(self.background, color, startpos, endpos, 1)
    
        # update the display
        self.screen.blit(self.background, (0, 0))
        pygame.display.update()
    
        if colorchange:
            # set color to blue
            color = [0, 0, 255]
    
        while True:
            # limit to 60 fps
            self.clock.tick(self.fps)

            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                    return
        
            if linepos <= self.screenwidth:
                if degree >= 360:
                    # a full turnaround, reset to zero (to prevent overflows)
                    degree = 0
            
                # calculate the exact sine value of that radian 
                sinval = math.sin(torad(degree))
                # maximize the value, and round it
                drawval = int(round(sinval * 100))
            
                if colorchange:
                    fract = drawval / 100.0
                    fract *= 255
                    fract = int(round(fract))
                
                    # changes the color of the next point
                    color[0] = abs(fract)
                    color[2] = 255 - abs(fract)
        
                # the coordinates of the next point drawn
                pointpos = (linepos, self.screenheight / 2 - drawval)
                # draw a pseudo line - a line with ending and starting poins being the same
                pygame.draw.line(self.background, color, pointpos, pointpos, 1)
                # ^^ that creates a point
            
                if thick:
                    # create additional points to be thicker
                    pointpos_upper = (pointpos[0], pointpos[1] - 1)
                    pointpos_lower = (pointpos[0], pointpos[1] + 1)
                    pygame.draw.line(self.background, color, pointpos_upper, pointpos_upper, 1)
                    pygame.draw.line(self.background, color, pointpos_lower, pointpos_lower, 1)
            
            
                # blit it on the screen and update the display
                self.screen.blit(self.background, (0, 0))
                pygame.display.update()
        
                # get the next degree
                degree += 1
                # set the y-axis point of the next point
                linepos += 1
    
    def visual_prime(self):
        """This displays primes. Numbers being primes are black,
        numbers not being primes white (change this?)"""
        import primebench
        noprimecolor = (255, 255, 255)
        primecolor = (0, 0, 0)
        primecolor, noprimecolor = noprimecolor, primecolor
    
        # blanking
        self.background.fill((0, 0, 0))
        self.screen.blit(self.background, (0, 0))
        pygame.display.update()
    
        for prime in primebench.rangeprime(0, self.screenwidth * self.screenheight):
            for event in pygame.event.get():
                    if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                        return
        
            # get the line of the point (y-axis)
            line = prime/self.screenwidth
            # get the position of the point in the line (x-axis)
            pos = prime - line * self.screenwidth
            coord = (pos, line)
        
            # drawing.. nearly as usual.. this time we use "dirty rectangles"
            drect = pygame.draw.line(self.background, primecolor, coord, coord, 1)
            # blit the surface on the screen
            self.screen.blit(self.background, (0, 0))
            # update _just_ the dirty rect... a great speedup compared to normal update
            pygame.display.update(drect)
            # just remove the "drect" here ^^ and you'll see
            
        while True:
            # limit to 60 fps
            self.clock.tick(self.fps)

            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                    return
    
    def cube(self, sizes=(100, 70), alpha=50, hexacol=(251, 224, 29), linecol=(0, 0, 0), pixel=1):
        """Creates the alpha'ed hexagons, and draws some lines.
        Ressembles the Gamecube logo, somehow."""
    
        # reblank the screen
        self.screen.fill((0, 0, 0))
    
        # create the sprites
        # the bigger
        bigrad = sizes[0]
        big = HexaSprite(radius=bigrad, color=hexacol, alpha=alpha)
        big.move_right(self.screenwidth/2-bigrad)
        big.move_down(self.screenheight/2-bigrad)
        self.screen.blit(big.surface, big.rect)
    
        # the smaller
        smallrad = sizes[1]
        small = HexaSprite(radius=smallrad, color=hexacol, alpha=alpha)
        small.move_right(self.screenwidth/2-smallrad)
        small.move_down(self.screenheight/2-smallrad)
        self.screen.blit(small.surface, small.rect)
    
        # get the coords of the center
        center = (self.screenwidth/2, big.rect.height/2 + self.screenheight/2-bigrad)
    
        # get the coordinates of the ending points
        ends = [
            # the right end point
            (big.rect.right, big.rect.height/2 + self.screenheight/2-bigrad),
            # the upper left point
            (big.rect.center[0] - big.coords[0][0], big.rect.center[1] - big.coords[2][1]),
            # the lower left point
            (big.rect.center[0] - big.coords[0][0], big.rect.center[1] + big.coords[2][1])
            ]
    
        # draws the lines
        for end in ends:
            pygame.draw.line(self.screen, linecol, center, end, pixel)
    
        # shows it
        pygame.display.update()
    
        while True:
            # limit to 10 fps
            self.clock.tick(10)

            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                    return
    
    def axe_alaska(self):
        """Make that AXE Alaska Logo
        Remember pygame coords: first x value, then y value"""
        
        def changed(move_def, pixels=1):
            # create a copy of the rect
            r = hs.rect
            # move
            move_def(pixels)
            # clamp it into the main window
            hs.rect = hs.rect.clamp(self.clamp_window)
            # check whether it was moved
            return r == hs.rect
    
        # okay, we first need the probability
        from entropy import Probability
        dirchange = Probability()
        # create X% of probability 
        dirchange.add(True, 10)
        dirchange.add(False, dirchange.remaining)

        #white = (255, 255, 255)
        #blue = (46, 144, 189)
        yellow = (251, 224, 29)
    
        # create the sprite
        hs = HexaSprite(radius=50, color=yellow, alpha=255)
        #temp
        #hs = UnsharpHexaSprite(radius=50, color=yellow, alpha=50)
        #/temp
        
        # set the sprite on position
        hs.move_right(self.screenwidth/2-50)
        hs.move_down(self.screenheight/2-50)
    
        move_right = True
        #hs.rotate(45)
    
        while True:
            # limit to 60 fps
            self.clock.tick(self.fps)

            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                    return
        
            # do we have to move right?
            if move_right:
                # yes... did we really moved?
                moved = changed(hs.move_right, 2)
                if not moved:
                    # no, we are at the border
                    move_right = not move_right
                    # so change the direction
            else:
                moved = changed(hs.move_left, 2)
                if not moved:
                    move_right = not move_right
                    continue
        
            hs.rotate(-7)
        
            # do we change the direction?
            if dirchange.random():
                # if true, we change, else not
                move_right = not move_right
        
            # show the stuff
            self.screen.fill((0, 0, 0))
            self.screen.blit(hs.surface, hs.rect)
            pygame.display.update()
    
    def textraise(self, step=10):
        color = [0, 0, 0]
        size = 5
        text = ['abc', 'def']
        
        fulliterations = 0
        
        raising  = True
        
        while True:
            # limit to 10 fps
            self.clock.tick(30)
            
            self.screen.fill((0, 0, 0))
            
            # config color
            if raising:
                color[0] += step
                color[1] += step
                color[2] += step
                if color[0] >= 255:
                    color = [255, 255, 255]
                    raising = False
            else:
                color[0] -= step
                color[1] -= step
                color[2] -= step
                if color[0] <= 0:
                    color = [0, 0, 0]
                    raising = True
                    
            # fonts
            # size control
            if raising:
                size += step / 2
            else:
                size -= step /2
            
            text_font = pygame.font.Font(None, size)
            
            cpt = text_font.render("Demo", True, color)
            x, y = text_font.size("Demo")
            xdeviation = x / 2
            ydeviation = y / 2
            
            position = ((self.screenwidth / 2) - xdeviation, (self.screenheight / 2) - ydeviation)
            self.screen.blit(cpt, position)
            
            pygame.display.flip()
            

            # handle events
            for event in pygame.event.get():
                if event.type == pyl.QUIT or event.type == pyl.KEYDOWN:
                    return

# Does the demos die on small problems?
deathtrap = False
# verbose output?
verbose = False

# convert deg values to rad values
torad = lambda deg: (deg * math.pi) / 180.0

def main():
    """Starts the demos""" 
    parser = optparse.OptionParser()
    parser.add_option("-a", "--all",
        dest="all", 
        default=True,
        action="store_true", 
        help="run all demos (which work mostly)")
    parser.add_option("-n", "--nointro",
        dest="intro", 
        default=True,
        action="store_false", 
        help="show intro")
    parser.add_option("-r", "--reblank",
        dest="frameskip", 
        default=8,
        action="store", 
        help="How many frames to skip in the reblanks",
        metavar="FRAMESKIP",
        type="int")
    parser.add_option("--critter",
        dest="critter", 
        default=False,
        action="store_true", 
        help="run simple critter demo")
    parser.add_option("--ant",
        dest="ant", 
        default=False,
        action="store_true", 
        help="show ant demo")
    parser.add_option("--popsquares",
        dest="popsquares", 
        default=False,
        action="store_true", 
        help="run popsquares effect")
    parser.add_option("--magnets",
        dest="magnets", 
        default=False,
        action="store_true", 
        help="run magnetic simulation")
    parser.add_option("--wave",
        dest="wave", 
        default=False,
        action="store_true", 
        help="run sine wave demo")
    parser.add_option("--alaska",
        dest="alaska", 
        default=False,
        action="store_true", 
        help="run AXE alaska demo")
    parser.add_option("--prime",
        dest="prime", 
        default=False,
        action="store_true", 
        help="run prime visualisation")
    parser.add_option("--cube",
        dest="cube",
        default=False,
        action="store_true",
        help="show kind of Gamecube logo")
    parser.add_option("--laser",
        dest="laser",
        default=False,
        action="store_true",
        help="fun with laser emulation")
    parser.add_option("--textraise",
        dest="textraise",
        default=False,
        action="store_true",
        help="a text effect")
    options, args = parser.parse_args()
    
    # We have to prepare display
    #prepare()
    app = Application()
    
    # run the demos
    #print options
    if options.intro:
        app.info("Screensaw " + __version__, "The Python GFX demonstration program")
        app.reblank(options.frameskip)
    
    if (options.critter or options.ant or options.popsquares or options.magnets
        or options.prime or options.wave or options.alaska or options.cube
        or options.laser or options.textraise):
        options.all = False
    
    if options.all or options.critter:
        if options.intro:
            app.info("Critter", "The first demo, a very simple one")
        app.critter()
        app.reblank(options.frameskip)
    
    if options.all or options.ant:
        if options.intro:
            app.info("Ant", "A more interessting demo, ressembles Snake")
        app.ant()
        app.reblank(options.frameskip)
    
    if options.all or options.popsquares:
        if options.intro:
            app.info("Popsquares", "The XScreenSaver Popsquares ported to Python")
        app.popsquares(slow=5)
        app.reblank(options.frameskip)
    
    if options.magnets:
        if options.intro:
            app.info("Magnets", "This should be a gravity demo.. unfinished")
        app.magnets()
        reblank(options.frameskip)
        
    if options.all or options.wave:
        if options.intro:
            app.info("Wave", "Draws a neat sine wave")
        app.sinwave(True, True)
        app.reblank(options.frameskip)
    
    if options.all or options.prime:
        if options.intro:
            app.info("Primes", "Visualizes primes")
        app.visual_prime()
        app.reblank(options.frameskip)
    
    if options.all or options.alaska:
        if options.intro:
            app.info("AXE Alaska", "Animated AXE Alaska logo")
        app.axe_alaska()
        #app.reblank(options.frameskip)
    
    if options.all or options.cube:
        if options.intro:
            app.info("Cube", "Creates a kind of Gamecube logo")
        app.cube()
        app.reblank(options.frameskip)
        
    if options.all or options.laser:
        if options.intro:
            app.info("Laser", "Work in progress")
        #app.laser(noblank=True, speed=5)
        app.multilaser(speed=1)
        #reblank(options.frameskip)
    
    if options.all or options.textraise:
        if options.intro:
            app.info("Textraise", "Work in progress")
        #app.laser(noblank=True, speed=5)
        app.textraise()
        #reblank(options.frameskip)

class HexaSprite(pygame.sprite.Sprite):
    """A sprite representing a hexagon"""
    lastdegree = 0
    def __init__(self, radius, color, alpha=255):
        """Create a blabal"""
        pygame.sprite.Sprite.__init__(self)
        
        self.coords = self.create_coords(radius)
        self.surface = pygame.Surface(self.size())
        self.rect = self.surface.get_rect()
        self.alpha = alpha
        
        pygame.draw.polygon(self.surface, color, self.coords, 0)
        self.surface.convert_alpha()
        self.surface.set_alpha(self.alpha)
        self.surface.set_colorkey((0, 0, 0), pyl.RLEACCEL)
        
        self.origsurface = self.surface
    
    def create_coords(self, radius):
        """Create the coordinates of a hexagon.
        I consider this as one of my better efforts.
        If you give it a radius, it will return coordinates, 
        x of the center being the center. Difficult to describe."""
        d = math.sqrt(radius ** 2 - (radius/2.0) ** 2)
        deviation = int(round(d))
    
        center_x = radius
        center_y = deviation
    
        middleleft = (center_x - radius, center_y)
        middleright = (center_x + radius, center_y)
    
        upperleft = (int(round(center_x - radius/2.0)), center_y - deviation)
        upperright = (int(round(center_x + radius/2.0)), center_y - deviation)
        lowerleft = (int(round(center_x - radius/2.0)), center_y + deviation)
        lowerright = (int(round(center_x + radius/2.0)), center_y + deviation)
    
        return (upperleft, upperright, middleright, lowerright, lowerleft, middleleft)
    
    def size(self):
        """Get the size of the hexagon"""
        left = self.coords[5][0]
        right = self.coords[2][0]
        width = right - left
    
        top = self.coords[0][1]
        buttom = self.coords[3][1]
        height = buttom - top
    
        return (width, height)
    
    def move_up(self, pixels=1):
        """Moving up...
        returns true if succeeded"""
        
        if self.rect.top > 0:
            self.rect.top -= pixels
            return True
        else:
            return False
    
    def move_down(self, pixels=1):
        self.rect.top += pixels
        
    def move_right(self, pixels=1):
        self.rect.left += pixels
    
    def move_left(self, pixels=1):
        self.rect.left -= pixels
    
    def rotate(self, degree):
        """Rotates the object"""
        rotation = degree + self.lastdegree
        center = self.rect.center
        
        if rotation > 360 or rotation < -360:
            # if the rotation got out of a full 360° movement, 
            #+reset it back to lower values
            rotation = rotation - self.lastdegree
        
        self.lastdegree = rotation
        
        self.surface = pygame.transform.rotate(self.origsurface, rotation)
        self.surface.convert_alpha()
        self.surface.set_alpha(self.alpha)
        self.rect = self.surface.get_rect()
        self.rect.center = center

class UnsharpHexaSprite(HexaSprite):
    """This child class represents a fuzzy hexagon"""
    def __init__(self, radius, color, alpha=50, gradient=(0, 10, 2)):
        """Constructs the hexagon.
        radius is obvious, color should also be.
        alpha is the alpha level each laver has (they sum)
        with alpha == 255 you can disable the unsharpness, and this
        class works just like it's parent class (but with a longer init time)
        last but not least, with gradiunt you can control the size
        and the amount of layers."""
        pygame.sprite.Sprite.__init__(self)
        
        surfaces = []
        
        self.coords = self.create_coords(radius)
        size = self.__size(self.coords)
        self.rect = pygame.Rect(0, 0, size[0], size[1])
        
        for layer in range(*gradient):
            coord = self.create_coords(radius - layer)
            surface = pygame.Surface(self.__size(coord))
            surface.convert_alpha()
            surface.set_alpha(alpha)
            surface.set_colorkey((0, 0, 0), pyl.RLEACCEL)
            pygame.draw.polygon(surface, color, coord, 0)
            surfaces.append(surface)
        
        surfsize = self.__size(self.coords)
        surfsize = surfsize[0] + 4, surfsize[1] + 4
        # four pixels buffer (two on each side)
        self.surface = pygame.Surface(surfsize)
        
        for surface in surfaces:
            ownrect = surface.get_rect()
            masterrect = self.surface.get_rect()
            xmin = ownrect.width / 2
            ymin = ownrect.height / 2
            xpoint = masterrect.width / 2 - xmin
            ypoint = masterrect.height / 2 - ymin
            self.surface.blit(surface, (xpoint, ypoint))
        
        # make a backup of the surface for ratating
        self.origsurface = self.surface
    
    def rotate(self, degree):
        """Rotates the object"""
        rotation = degree + self.lastdegree
        center = self.rect.center
        
        if rotation > 360 or rotation < -360:
            # if the rotation got out of a full 360° movement, 
            #+reset it back to lower values
            rotation = rotation - self.lastdegree
        
        self.lastdegree = rotation
        
        self.surface = pygame.transform.rotate(self.origsurface, rotation)
        self.rect = self.surface.get_rect()
        self.rect.center = center

    def __size(self, coords):
        """Internal: Same as size, but this one takes coordinates.
        You shouldn't use this from outside this class."""
        left = coords[5][0]
        right = coords[2][0]
        width = right - left
    
        top = coords[0][1]
        buttom = coords[3][1]
        height = buttom - top
    
        return (width, height)
    
if __name__ == '__main__':
    main()