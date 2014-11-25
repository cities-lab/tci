"""Given related pairs, group into clusters having common partners.

Example:

1--5--3  2--4

pairs
a    b
1    5
2    4
3    5
4    2
5    1
5    3

groups
id  group
1   1
2   2
3   1
4   2
5   1
"""

import csv

# Load pairs from file
print 'Loading pairs...'
f = open('C:\\Users\\huajie\\Desktop\\empDensityLEHD2000.csv')
reader = csv.reader(f)
reader.next()  # skip header

pairs = {}
for a, b, v1, v2, v3, v4 in reader:
    if a not in pairs:
        pairs[a] = set()
    pairs[a].add(b)
    # add a to any set b is in and v.v.
    for item in pairs:
        if b in pairs[item]:
            pairs[item].add(a)
        if a in pairs[item]:
            pairs[item].add(b)
print '{} items to assign'.format(len(pairs))        

grouped = set()  # these items are grouped 
groups = {}  # holds grouping results
group = 1
for a in sorted(pairs):
    B = pairs[a]
    # check if any related items already grouped
    if a in grouped or [b for b in B if b in grouped]:
        for g in groups:
            if a in groups[g] or [b for b in B if b in groups[g]]:
                groups[g].add(a)
                grouped.add(a)
                for b in B:
                    groups[g].add(b)
                    grouped.add(b)
                break
    else:
        groups[group] = set()
        groups[group].add(a)
        grouped.add(a)
        for b in B:
            groups[group].add(b)
            grouped.add(b)
        group += 1

f.close()

# Get total items assigned
n = 0
for g in groups:
    n += len(groups[g])

# Write out to groups
print 'Writing groups...'
f = open('C:\\Users\\huajie\\Desktop\\groupsLEHD2000.csv', 'wb')
f.write('id,group\n')
for group in sorted(groups):
    for item in groups[group]:
        f.write('{},{}\n'.format(item, group))
print '{} items placed into {} groups'.format(n, len(groups))
f.close()
