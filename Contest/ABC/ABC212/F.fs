let MOD = 998244353L
let P = stdin.ReadLine() |> int64

let rec fac x = 
    if x = 1L then 1L else (x * (fac (x-1L))) % MOD

(P-2L) * (P-1L)