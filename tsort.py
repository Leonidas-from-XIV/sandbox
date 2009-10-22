#!/usr/bin/env python
# -*- coding: UTF-8 -*-
"""tsort.py - Topological sort in Python.
This program should behave like 'tsort' from the coreutils.

written 2007, 2009 by Marek Kubica
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
http://de.wikipedia.org/wiki/Topologische_Sortierung because it is simple and
non-cyclic.

To sort these elements topological you need to create node structure first:
>>> nodes = create_nodes(depends)
>>> print all_in_order(nodes)

That's all. If you have any comments or improvements, feel free to contact
the author."""

class GraphNode(object):
    """The representation of a node"""
    def __init__(self, value):
        self.value = value
        self.dependents = list()

    def __repr__(self):
        """A nice representation for programmers"""
        return '<GraphNode value=%s dependents=%d>' % (
                    self.value, len(self.dependents)
                )

    def __str__(self):
        """A nice representation for user output"""
        return self.value

    def __eq__(self, other):
        """Are two GraphNodes equal?"""
        # they are equal as long as their 'value' is equal
        return self.value == other.value

def get_from_cache(cache, element):
    """Gets a node from a list (cache) if there is already the same node in 
    this list. If not, give back the original node"""
    try:
        return cache[cache.index(element)]
    except ValueError:
        return element

def create_nodes(depends):
    """Create from the single string dictionary a structured list with
    dependencies."""
    nodes = list()

    # iterate through all items of the dict
    for node_name, dependency_names in depends.iteritems():
        # create a node by this name
        node = GraphNode(node_name)
        # check whether there already exists one by this name
        node = get_from_cache(nodes, node)

        # now, let's check the dependencies
        for dependency_name in dependency_names:
            # create and check dependent nodes - again
            subnode = GraphNode(dependency_name)
            subnode = get_from_cache(nodes, subnode)

            # add this node as 'dependent' to the node it belongs to
            node.dependents.append(subnode)
            # add this subnode to the list of all nodes
            if subnode not in nodes:
                nodes.append(subnode)

        # add the node to the list of all nodes
        if node not in nodes:
            nodes.append(node)
    return nodes

def chunk_in_order(nodes):
    """Returns a list of items which come first in the topological sorted
    items - that is, these items that do not depend on anything."""
    # get all nodes which do not depend on anything
    starting_nodes = [node for node in nodes if len(node.dependents) == 0]

    # delete all dependencies on these nodes
    for delete_candidate in starting_nodes:
        for node in nodes:
            if delete_candidate in node.dependents:
                node.dependents.remove(delete_candidate)
        # remove these nodes from the list of all nodes
        nodes.remove(delete_candidate)

    # return the nodes
    return starting_nodes

def has_zero_dependents(nodes):
    """Check whether there are any nodes which do notdepend on anything"""
    number = len([node for node in nodes if len(node.dependents) == 0])
    return number > 0

def all_in_order(nodes):
    """Returns all nodes topologically sorted"""
    order = list()
    while has_zero_dependents(nodes):
        chunk = chunk_in_order(nodes)
        order.extend(chunk)
    return order

def main():
    """Get the input from the user"""
    depends = dict()

    try:
        while True:
            line = raw_input('> ')
            try:
                dependency, node = line.split(None, 1)
            except ValueError:
                # empty line
                continue
            depends[node] = depends.get(node, []) + [dependency]
    except EOFError:
        # add a new line
        print

    # create the proper dependency structure
    node_structure = create_nodes(depends)
    # display it in a sorted way
    for element in all_in_order(node_structure):
        print element

if __name__ == '__main__':
    main()
