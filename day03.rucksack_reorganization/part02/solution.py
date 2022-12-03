#!/usr/bin/env python3
from functools import reduce
import sys

def get_priority(item_type):
    item_type_ord = ord(item_type)
    if item_type_ord < ord('a'):
        return 27 + (item_type_ord - ord('A'))
    else:
        return 1 + (item_type_ord - ord('a'))

chunk_size = 3
chunk_sets = []
priorities_sum = 0

for line in sys.stdin.readlines():
    if line[-1] == '\n':
        line = line[:-1]

    item_types = frozenset(line)
    chunk_sets.append(item_types)
    print('from line: ', item_types)

    if (len(chunk_sets) >= chunk_size):
        common_item_types = reduce(frozenset.intersection, chunk_sets[1:], chunk_sets[0])
        assert len(common_item_types) == 1
        common_item_type = list(common_item_types)[0]
        print('common item type:', common_item_type)
        priorities_sum += get_priority(common_item_type)
        chunk_sets.clear()

print('priorities sum: %d' % priorities_sum)
