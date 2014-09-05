#!/usr/bin/env python
import re
import sys

class Node:
    def __init__(self, term = None):
        self.term = term
        self.left = None
        self.right = None
        self.trans = []
        self.id = None
        self.accept = False

def to_bin(s):
    res = []
    for i in range(0, len(s), 8):
        x = s[i:i+8]
        x += '0'*(8 - len(x))
        a = 0
        for j in range(8):
            a *= 2
            a += ord(x[j]) - ord('0')
        res.append(a) #chr(a))
    return res

nodes = []

def insert(node, sym, bits):
    if len(bits) == 0:
        node.term = sym
        return
    else:
        if bits[0] == '0':
            if node.left is None:
                node.left = Node()
            child = node.left
        else:
            if node.right is None:
                node.right = Node()
            child = node.right
        insert(child, sym, bits[1:])

def traverse(node, bits, syms, start_node, root, depth):
    if depth == 4:
        if 256 in syms:
            syms = []
            node = None
        start_node.trans.append((node, bits, syms))
        return

    if node.term is not None:
        node = root

    def go(node, bit):
        nbits = list(bits)
        nbits.append(bit)
        nsyms = list(syms)
        if node.term is not None:
            nsyms.append(node.term)
        traverse(node, nbits, nsyms, start_node, root, depth + 1)

    go(node.left, 0)
    go(node.right, 1)

idseed = 0

def dfs_setid(node, prefix):
    if node.term is not None:
        return
    if len(prefix) <= 7 and [1] * len(prefix) == prefix:
        node.accept = True
    global idseed
    node.id = idseed
    idseed += 1
    dfs_setid(node.left, prefix + [0])
    dfs_setid(node.right, prefix + [1])

def dfs(node, root):
    if node is None:
        return
    traverse(node, [], [], node, root, 0)
    dfs(node.left, root)
    dfs(node.right, root)

NGHTTP2_HUFF_ACCEPTED = 1
NGHTTP2_HUFF_SYM = 1 << 1
NGHTTP2_HUFF_FAIL = 1 << 2

def dfs_print(node):
    if node.term is not None:
        return

    k = 0
    for nd, bits, syms in node.trans:
        outlen = len(syms)
        flags = 0
        if outlen == 0:
            out = -1
        else:
            assert(outlen == 1)
            out = syms[0]

        if nd is None:
            id = 0
            flags = 'fail'
        else:
            id = nd.id
            if id is None:
                # if nd.id is None, it is a leaf node
                id = 0
                flags = 'accept'
            elif nd.accept:
                flags = 'accept'
            else:
                flags = 'not_accept'

        print '''\
decode_substate{nodeid}({k}) ->
    {{{flag}, {id}, {out}}}{tail}\
'''.format(nodeid=node.id, k=k, id=id, flag=flags, out=out if out != -1 else 'no_symbol', tail=';' if k < 15 else '.')

        k += 1

    print ''

    dfs_print(node.left)
    dfs_print(node.right)

symbol_tbl = [(None, 0) for i in range(257)]
tables = {}

root = Node()

for line in sys.stdin:
    m = re.match(r'.*\(\s*(\d+)\)\s+([|01]+)\s+(\S+)\s+\[\s*(\d+)\].*', line)
    if m:
        #print m.group(1), m.group(2), m.group(4)
        if len(m.group(3)) > 8:
            raise Error('Code is more than 4 bytes long')
        sym = int(m.group(1))
        bits = re.sub(r'\|', '', m.group(2))
        nbits = int(m.group(4))
        assert(len(bits) == nbits)
        binpat = to_bin(bits)
        assert(len(binpat) == (nbits+7)/8)
        symbol_tbl[sym] = (binpat, nbits, m.group(3), bits)
        #print "Inserting", sym
        insert(root, sym, bits)

dfs_setid(root, [])
dfs(root, root)

for i in range(257):
    pat = list(symbol_tbl[i][0])
    pat += [0]*(4 - len(pat))
    print '''\
symbol(<<{}>>) ->
    <<2#{}:{}>>{}\
'''.format(i, symbol_tbl[i][3], symbol_tbl[i][1], ';' if i < 256 else '.')

print ''

for i in range(256):
    print '''\
decode_state({id}, I) ->
    decode_substate{id}(I){tail}'''.format(id=i, tail=';' if i < 255 else '.')
print ''

dfs_print(root)
