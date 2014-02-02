import sys

def divisible(num, factor):
    return num % factor == 0

def divide(num, a, b):
    if divisible(num, a) and divisible(num, b):
        return "FB"
    elif divisible(num, a):
        return "F"
    elif divisible(num, b):
        return "B"
    else:
        return str(num)

def process_line(a, b, n):
    for num in range(1, n+1):
        yield divide(num, a, b)

def main():
    with open(sys.argv[1]) as f:
        for line in f:
            a, b, n = [int(i) for i in line.split()]
            print(" ".join(r for r in process_line(a, b, n)))

if __name__ == '__main__':
    main()
