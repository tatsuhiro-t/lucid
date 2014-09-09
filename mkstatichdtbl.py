#!/usr/bin/env python
import re, sys

entries = []
for line in sys.stdin:
    m = re.match(r'(\d+)\s+(\S+)\s+(\S.*)?', line)
    val = m.group(3).strip() if m.group(3) else ''
    entries.append((int(m.group(1)), m.group(2), val))

for ent in entries:
    quot = '"' if ent[2] else ''
    print '''\
static_match(<<"{name}">>, <<{quot}{value}{quot}>>) ->
    {{match, {idx}}};
static_match(<<"{name}">>, <<_/binary>>) ->
    {{name_match, {idx}}};\
'''.format(idx=ent[0], name=ent[1], value=ent[2], quot=quot)

print '''\
static_match(<<_/binary>>, <<_/binary>>) ->
    nomatch.
'''

for ent in entries:
    quot = '"' if ent[2] else ''
    print '''\
get({}, _Context) ->
    {{<<"{}">>, <<{}{}{}>>}};\
'''.format(ent[0], ent[1], quot, ent[2], quot)

