import sys
from functools import lru_cache
from itertools import cycle


@lru_cache(maxsize=None)
def factrize(n, s):
    return [i for i in range(s, n + 1) if n % i == 0]


class Code:
    def __init__(self, bits=8):
        self.code = []
        self.num = 0
        self._move = 0
        self.cell_size = 1 << bits

    def copy(self):
        code = Code()
        code.code = self.code[:]
        code.num = self.num
        code._move = self._move
        code.cell_size = self.cell_size
        return code

    @property
    def move(self):
        c = '><'[self._move]
        self._move = 1 - self._move
        return c

    def __eq__(self, other):
        return self.code == other.code

    def _add(self, n):
        self.num = (self.num + n) % self.cell_size
        if n < 0:
            self.code.append(['-', -n])
        elif n > 0:
            self.code.append(['+', n])

    def __add__(self, n):
        code = self.copy()
        code._add(n)
        return code

    def _mul(self, n):
        assert n > 0
        assert self.num > 0
        if self.num <= self.cell_size // 2:
            self.code.append(['[-%s' % self.move, 1])
            self.code.append(['+', n])
            self.code.append(['%s]%s' % (self.move, self.move), 1])
        else:
            self.code.append(['[+%s' % self.move, 1])
            self.code.append(['-', n])
            self.code.append(['%s]%s' % (self.move, self.move), 1])
        self.num = (self.num * n) % self.cell_size

    def __mul__(self, n):
        code = self.copy()
        code._mul(n)
        return code

    @staticmethod
    def _size(code):
        return sum(len(s) * n for s, n in code)

    @property
    def score(self):
        return self._size(self.code)

    def __str__(self):
        return ''.join(s * n for s, n in self.code)


best_score = 0
best_code = None

def dfs(num, code, depth):
    global best_score, best_code
    num = num % code.cell_size
    d = min([code.num - num, -(code.cell_size - num + code.num)], key=abs)
    best = code + d
    if best_code is None or best.score < best_score:
        best_score = best.score
        best_code = best
    if best.score > best_score or depth <= 0:
        return
    max_range = min(num, 100)
    for a in range(max_range, -max_range-1, -1):
        c = code + a
        if c.score >= best_score:
            continue
        if c.num == 0:
            continue
        for m in range(2, max_range+1):
            dfs(num, c * m, depth - 1)


def main():
    global best_score, best_code
    n = int(sys.argv[1])
    bits = int(sys.argv[2]) if len(sys.argv) > 2 else 8
    pre = ''
    for max_depth in range(5):
        best_score = 0
        best_code = None
        dfs(n, Code(bits), max_depth)
        assert best_code.score == best_score
        cs = str(best_code)
        if cs != pre:
            print(max_depth, best_score, cs)
            pre = cs


if __name__ == '__main__':
    main()
