# reverse

## bf

```
入力取得
>>>>,[>+>,]
0 0 0 0 x 1 y 1 z 1 0
                    _
<f[-
  フラグを辿って値を移動
  <[-
    <[<<]
    <+>
    >>[>>]
    <
  ]
  全体を2個右に移動
  <[[->>+<<]<]
  >>>[>]<
]
ヘッドに移動
<<<[<<]>>
出力
[.>>]
```

```
struct C {
    x: Cell,
    f: Cell,
}

fn main(arr: [C]) {
    arr += 1;
    while comma(*arr.x) {
        *arr.f += 1;
        arr += 1;
    }
    while {arr -= 1; *arr.f} {
        *arr.f -= 1;
        while *arr.x {
            *arr.x -= 1;
            while {arr -= 1; *arr} {}
            *arr.x += 1;
            while {arr += 1; *arr} {}
        }
        while {arr -= 1; *arr} {
            arr[1] += *arr;
        }
        while {arr += 1; *arr} {}
    }
    while {arr -= 1; *arr.x} {}
    while {arr += 1; *arr.x} {
        dot(*arr.x);
    }
}
```
