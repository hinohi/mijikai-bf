import sys
import enum

def bf_len(bf):
    return sum(len(s) * c for s, c in bf)

def bf_dumps(bf):
    return ''.join(s * c for s, c in bf)


class OverflowBehavior(enum.Enum):
    wrap = enum.auto
    abort = enum.auto


class BFInterpreter:
    def __init__(self):
        self.words = 1
        self.eof = -1  # 0 or -1
        self.ob = OverflowBehavior.wrap

    @property
    def cell_size(self):
        return 1 << (self.words * 8)

    @property
    def unsinged_eof(self):
        return self.eof & (self.cell_size - 1)

    def dist(self, start, goal):
        if start == goal:
            return []
        if self.ob is OverflowBehavior.wrap:
            if start < goal:
                pass

    def set_zero(self, use_read):
        if use_read:
            if self.eof == -1 and self.ob is OverflowBehavior.abort:
                return [[BF_ZERO, 1]]
            else:
                bf = [[BF_GET, 1]]
                if self.eof == -1:
                    bf.append([BF_INC, 1])
                return bf
        else:
            return [[BF_ZERO, 1]]

    def best_zero(self, start, use_read):
        return min([self.best_zero(use_read=True), [[BF_DEC, start]]], key=bf_len)

    def best_set(self, start, goal, use_read=False):
        if start == goal:
            return []
        if start + 1 == goal:
            return [[BF_INC, 1]]
        if start - 1 == goal:
            return [[BF_DEC, 1]]
        if goal == 0:
            return self.best_zero(start, use_read)
        if self.ob is OverflowBehavior.wrap:
            if start < goal:
                bf0 = [[BF_INC, goal - start]]
                bf1 = self.best_zero(start, use_read) + [[BF_DEC, self.cell_size - goal]]

BF_INC = '+'
BF_DEC = '-'
BF_RIGHT = '>'
BF_LEFT = '<'
BF_OPEN = '['
BF_CLOSE = ']'
BF_GET = ','
BF_PUT = '.'
BF_ZERO = '[-]'

bfi = BFInterpreter()


def _one_cell_use_read(ords, start):
    bf = []
    n = len(ords)
    for i in range(n - 1):
        pass

def _one_cell_without_read(ords, start):
    bf = []
    for b in ords:
        if start < b:
            bf.append([BF_INC, b - start])
        elif start > b:
            bf.append([BF_DEC, start - b])
        bf.append([BF_PUT, 1])
        start = b
    return bf, sum(bf, key=lambda x: x[1])


def one_cell(ords, start=0, use_read=False):
    if use_read:
        bf0, s0 = _one_cell_use_read(ords, start)
        bf1, s1 = _one_cell_without_read(ords, start)
        if s0 < s1:
            return bf0, s0
        else:
            return bf1, s1
    else:
        return _one_cell_without_read(ords, start)


def single_mul(ords):
    best_len = float('inf')
    best_bf = []
    for m in range(2, 16):
        bf = [[BF_INC, m], [BF_OPEN, 1], [BF_DEC, 1]]
        for b in ords:
            bf.append([BF_RIGHT, 1])
            if b % m < 1 + (m - b % m):
                bf.append([BF_INC, b // m])
            else:
                bf.append([BF_INC, b // m + 1])
        bf.append([BF_LEFT, len(ords)])
        bf.append([BF_CLOSE, 1])
        for b in ords:
            bf.append([BF_RIGHT, 1])
            if b % m < 1 + (m - b % m):
                bf.append([BF_INC, b % m])
            else:
                bf.append([BF_DEC, m - b % m])
            bf.append([BF_PUT, 1])
        l = bf_len(bf)
        if best_len > l:
            best_len = l
            best_bf = bf
    return best_bf


def main():
    s = sys.argv[1]
    b = [ord(i) for i in s]
    print(b)
    print(bf_dumps(single_mul(b)))


if __name__ == '__main__':
    main()
