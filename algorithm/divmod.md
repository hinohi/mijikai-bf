# divmod

## 概要

1cell対1cellの徐算を行い、商とあまりを求める。

## アルゴリズム

```bf
ndrqXY
n[n->d-[>r+>>X]>Yr?[r+[-<d+>]>q+>>Y]Y<<<<<n]
```

## 前提メモリ条件

+ n: 被除数
+ d: 除数(2以上)
+ X, Y, r: 0

## 処理後のセル

+ n: 0
+ d: d-n%d
+ r: n%d
+ q: q + n/d
+ X, Y: 0

## 擬似コード

```
def divmod (
    n: cell
    d: cell
    r: cell
    q: cell
    x: TERM
    y: TERM
)
where (
    d >= 2
    r == 0
    &d - &r == &x - &y
)
then (
    n: 0
    d: d - n % d
    r: n % d
    q: q + n / d
)
    n {
        n -= 1
        d -= 1
        d [
            r += 1
            x
        ] (d|x)
        *(&r - &d)
        (r|y) [
            r += 1
            d += r
            q += 1
            y
        ] y
    }
```
