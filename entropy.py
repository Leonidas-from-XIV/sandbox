#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import random

class Probability(object):
    remaining = 100
    field = []
    
    def add(self, option, factor):
        if factor <= self.remaining:
            self.remaining -= factor
            self.field.extend([option] * factor)
        else:
            self.field.extend([option] * factor - self.remaining) 
            self.remaining = 0
    
    def random(self):
        seed = len(self.field)
        if seed < 100:
            self.add(None, 100 - seed)
        return random.choice(self.field)
        

