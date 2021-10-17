let MOD = 998244353L
let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int
let B = stdin.ReadLine().Split() |> Array.map int
let dp = 
    [|
        for i in 1..N ->
            [|for j in 0..3000 -> 0L|] 
    |]

for i in A.[0]..B.[0] do
    dp.[0].[i] <- 1L

for i in 1..N-1 do
    let mutable sum = 0L
    for j in 0 .. 3000 do
        sum <- (sum + dp.[i-1].[j]) % MOD
        if A.[i] <= j && j <= B.[i]
        then
            dp.[i].[j] <- sum

let mutable ans = 0L

for i in 0..3000 do
    ans <- (ans + dp.[N-1].[i]) % MOD

printfn "%d" ans