import sys
from math import sqrt
from functools import lru_cache
from itertools import cycle


@lru_cache(maxsize=None)
def factrize(n):
    ret = []
    i = 2
    for i in range(2, int(sqrt(n)) + 1):
        if n % i == 0:
            ret.append(i)
            j = n // i
            if j != i:
                ret.append(j)
    return ret


class Code:

    MUL_OVERHEAD = 6

    def __init__(self, bits=8):
        self.code = []
        self.log = []
        self.num = 0
        self._move = 0
        self.cell_size = 1 << bits

    def copy(self):
        code = Code()
        code.code = self.code[:]
        code.log = self.log[:]
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
            self.log.append(f'-{-n}')
        elif n > 0:
            self.code.append(['+', n])
            self.log.append(f'+{n}')

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
        self.log.append(f'*{n}')

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


class DepthFirstSearch:

    def __init__(self, null_code, goal, max_depth):
        self.null_code = null_code
        self.goal = goal % self.null_code.cell_size
        self.max_depth = max_depth
        self.best_code = None
        self.best_score = None

    def comp(self, code):
        d = min([self.goal - code.num, self.goal - code.num - code.cell_size], key=abs)
        code = code + d
        if self.best_code is None or code.score < self.best_score:
            self.best_code = code
            self.best_score = code.score

    def dfs(self):
        self._dfs(self.null_code, 1)
        return self.best_code

    def _dfs(self, code, depth):
        self.comp(code)
        if depth >= self.max_depth:
            return
        add = 0
        while code.score + abs(add) + 2 + code.MUL_OVERHEAD < self.best_score:
            added = code + add
            if abs(added.num) > 1:
                mul = 2
                while added.score + mul + code.MUL_OVERHEAD < self.best_score:
                    self._dfs(added * mul, depth + 1)
                    mul += 1
            if add <= 0:
                add = -add + 1
            else:
                add = -add


def main():
    n = int(sys.argv[1])
    bits = int(sys.argv[2]) if len(sys.argv) > 2 else 8
    pre = ''
    for max_depth in range(1, 5):
        dfs = DepthFirstSearch(Code(bits), n, max_depth)
        code = dfs.dfs()
        cs = str(code)
        if cs != pre and len(cs) < 1000:
            print(max_depth, code.score, cs, code.log)
        else:
            print(max_depth, code.score)
        pre = cs


if __name__ == '__main__':
    main()
