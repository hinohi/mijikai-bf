import sys
from functools import lru_cache


@lru_cache(maxsize=None)
def factrize(n, s):
    return [i for i in range(s, n + 1) if n % i == 0]


def score_func(f):
    return sum(f), len(f)


def dfs(n, s, f):
    if n == 1:
        return score_func(f), f
    if s <= 1:
        f = f + [n]
        return score_func(f), f
    best = None
    best_f = None
    for fact in factrize(n, f[-1] if f else 2):
        score, ff = dfs(n // fact, s - 1, f + [fact])
        if score is None:
            break
        if best is None or score < best:
            best = score
            best_f = ff
    return best, best_f


def main():
    n = int(sys.argv[1])
    pre = []
    s = 1
    while True:
        score, f = dfs(n, s, [])
        if pre == f:
            break
        print(s, score, f)
        pre = f
        s += 1


if __name__ == '__main__':
    main()
