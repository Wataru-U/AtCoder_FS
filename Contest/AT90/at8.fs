let N = stdin.ReadLine() |> int
let str = stdin.ReadLine()
let MOD = (1e+9 |> int64) + 7L
let rec calc (A:int64[]) n = 
    if n = N then A.[6]
    else
        let c = str.[n]
        match c with 
        | 'a' -> 
            A.[0] <- (A.[0] + 1L) % MOD
            calc A (n+1)
        | 't' ->
            A.[1] <- (A.[0] + A.[1]) % MOD
            calc A (n+1)
        | 'c' ->
            A.[2] <- (A.[1] + A.[2]) % MOD
            calc A (n+1)
        | 'o' ->
            A.[3] <- (A.[2] + A.[3]) % MOD
            calc A (n+1)
        | 'd' ->
            A.[4] <- (A.[3] + A.[4]) % MOD
            calc A (n+1)
        | 'e' ->
            A.[5] <- (A.[4] + A.[5]) % MOD
            calc A (n+1)
        | 'r' ->
            A.[6] <- (A.[5] + A.[6]) % MOD
            calc A (n+1)
        | _ -> calc A (n+1)

calc [|for i in 0..6 -> 0L|] 0
|> printfn "%d"