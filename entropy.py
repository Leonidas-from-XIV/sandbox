#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""This module contains some abilities to work with the random
written by Marek Kubica"""
import random

class Probability(object):
    """With this class you can create values with a probability
    For now values lover than 1% are not supported"""
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
        

