#!/usr/bin/env python
#! -*- coding: UTF-8 -*-
"""tsort.py - Topological sort in Python.
This program should behave like 'tsort' from the coreutils.

written 2007 by Marek Kubica
This program is free software, you can use and modify it under the terms of
the revised BSD license.

The data structure it consumes looks like this:

depends = {
    'Pullover' : ['Unterhemd'],
    'Hose' : ['Unterhose'],
    'Mantel' : ['Pullover'],
    'Schuhe' : ['Hose', 'Socken'],
}

This example is taken from Wikipedia,
http://de.wikipedia.org/wiki/Topologische_Sortierung
because it is non-cyclic."""

class GraphNode(object):
    """The representation of a node"""
    def __init__(self, value):
        self.value = value
        self.forerunner = list()

    def __repr__(self):
        return '<GraphNode value=%s forerunner=%d>' % (self.value, len(self.forerunner))

    def __str__(self):
        return self.value

    def __eq__(self, other):
        result = True if self.value == other.value else False
        return result

def get_from_cache(cache, element):
    try:
        return cache[cache.index(element)]
    except ValueError:
        return element

def create_nodes(depends):
    nodes = list()
    for key, values in depends.iteritems():
        node = GraphNode(key)
        node = get_from_cache(nodes, node)

        for value in values:
            subnode = GraphNode(value)
            subnode = get_from_cache(nodes, subnode)

            node.forerunner.append(subnode)
            if subnode not in nodes:
                nodes.append(subnode)

        if node not in nodes:
            nodes.append(node)
    return nodes

def chunk_in_order(nodes):
    starting_nodes = [node for node in nodes if len(node.forerunner) == 0]
    for delete_candidate in starting_nodes:
        for node in nodes:
            if delete_candidate in node.forerunner:
                node.forerunner.remove(delete_candidate)
        nodes.remove(delete_candidate)

    return starting_nodes

def has_zero_forerunners(nodes):
    number = len([node for node in nodes if len(node.forerunner) == 0])
    result = True if number > 0 else False
    return result

def all_in_order(nodes):
    order = list()
    while has_zero_forerunners(nodes):
        chunk = chunk_in_order(nodes)
        order.extend(chunk)
    return order

def main():
    depends = {}

    try:
        while True:
            line = raw_input('> ')
            try:
                node, dependencies = line.split(None, 1)
            except ValueError:
                # empty line
                continue
            depends[node] = depends.get(node, []) + dependencies.split()
    except EOFError:
        pass

    node_structure = create_nodes(depends)
    for element in all_in_order(node_structure):
        print element

if __name__ == '__main__':
    main()
