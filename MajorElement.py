import sys
from collections import defaultdict

def process(req, l):
    hist = defaultdict(int)
    for num in l:
        hist[num] += 1
        if hist[num] > req:
            return num
    return "None"

def process_line(l):
    li = l.split(',')
    req = len(li) // 2
    return process(req, li)

def main():
    with open(sys.argv[1]) as f:
        for l in f:
            l = l.strip()
            if l:
                print(process_line(l))

if __name__ == '__main__':
    main()
