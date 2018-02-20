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
