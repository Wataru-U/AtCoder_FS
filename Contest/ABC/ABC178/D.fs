let S = stdin.ReadLine() |> int

let MOD = 7L + (1e+9 |> int64)

let mutable dp = [|for i in 0..S -> 0L|]
let mutable sum = [|for i in 0..S -> 0L|]

for i in 3..S do
    dp.[i] <- (1L + sum.[i-3]) % MOD
    sum.[i] <- (sum.[i-1] + dp.[i]) % MOD

printfn "%d" dp.[S]