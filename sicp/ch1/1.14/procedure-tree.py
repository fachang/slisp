#!/usr/bin/env python

'''
Generated DOT file of the process tree
Usage: procedure-tree.py | dot -T png -o procedure-tree.png
'''


def first_denomination(kinds_of_coins):
    return [1, 5, 10, 25, 50][kinds_of_coins-1]


def cc_graph(amount, kinds_of_coins, node):
    print '    %s [label="(cc %s %s)"];' % (node, amount, kinds_of_coins)
    if amount == 0:
        return 1
    elif amount < 0 or kinds_of_coins == 0:
        return 0
    else:
        print '    %s -> %s;' % (node, node + 'l')
        print '    %s -> %s;' % (node, node + 'r')
        return (cc_graph(amount, kinds_of_coins - 1, node + 'l') +
                cc_graph(amount - first_denomination(kinds_of_coins),
                         kinds_of_coins,
                         node + 'r'))


def count_change_graph(amount):
    print 'digraph {'
    cc_graph(amount, 5, 's')
    print '}'

count_change_graph(11)
