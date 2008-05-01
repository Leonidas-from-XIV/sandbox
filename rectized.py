#!/usr/bin/env python
# -*- encoding: latin-1 -*- 

import random
import pygame
import pygame.locals as pyl

# screensize
side = 400
alpha_range = (10, 240)
color_range = (10, 240)

class AlphaRect(pygame.sprite.Sprite):
    def __init__(self, size, color, alpha):
        """Initializes the sprinte"""
        # init the parent
        pygame.sprite.Sprite.__init__(self)
        
        # create a surface to draw on
        self.surface = pygame.Surface((size, size))
        # fill it with the color
        self.surface.fill(color)
        # convert to alpha-able surface
        self.surface.convert_alpha()
        # set the alpha value
        self.surface.set_alpha(alpha)

class Application:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((side, side))
        # create a clock to control the FPS
        self.clock = pygame.time.Clock()
        self.fps = 30
    
    def draw_single(self):
        position_x = random.randint(0, side)
        position_y = random.randint(0, side)
        position = (position_x, position_y)
        
        if position_x > position_y:
            biggest = position_x
        else:
            biggest = position_y
        
        max_size = side - biggest
        min_size = 10
        if max_size > 0.6 * side:
            max_size /= 3
        elif max_size <= min_size:
            max_size = min_size + 1
        
        size = random.randint(min_size, max_size)
        
        color = (0, 0, random.randint(*color_range))
        
        alpha = random.randint(*alpha_range)
        sprite = AlphaRect(size, color, alpha)
        self.screen.blit(sprite.surface, position)
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
                
            # draw the clock
            self.draw_single()

if __name__ == '__main__':
    app = Application()
    app.main_loop()
