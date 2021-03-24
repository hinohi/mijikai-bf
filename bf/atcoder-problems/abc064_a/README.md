# ABC 064

url: https://atcoder.jp/contests/abc064/tasks/abc064_a

## 問題概要

1以上9以下の3整数 `r`, `g`, `b` が与えられる。
3桁の整数 `rgb` が4の倍数なら `YES` 違いなら `NO` を出力せよ。 

## example

### 1

```
4 3 2
```

```
YES
```

### 2

```
2 3 4
```

```
NO
```

## 疑似コード

```
ues algorithm::divmod

def mul10 (
    n: cell
    t: cell
)
where (
    t == 0
)
then (
    n: n * 10
    t: 0
)
    t += n * 2
    n += t * 5

def read (
    n: cell
    f: cell
    t: cell
)
where (
    n == 0
    f == 0
)
then (
    f: 0
    t: 0
)
    f += 3
    f {
        f -= 1
        mul10 n t
        get t
        t -= '0'
        n += t
        get null
    }

def print_yes_if_mod4_else_no (
    r: cell
    t: cell
)
where (
    t == 0
)
    t += 1
    r [
        print "NO"
        null 
    ] (r|null)
    (t|null) [
        print "YES"
        null
    ] null

def main (
    n: cell
    r: cell
)
    read n null null
    divmod n 4 r
    print_yes_if_mod4_else_no r
```
