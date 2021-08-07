let n = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int64 |> Array.sort
let MOD = 998244353L

let mutable Ans = 0L
let A_2 = A |> Array.map (fun x -> (x * x) % MOD)

for i in A_2 do
    Ans <- (Ans + i) % MOD

let rec culc (ans:int64) (sum:int64) num (e:int64) = 
    if num = n then ans
    else
        let ns = (A.[num - 1] + (sum * 2L) % MOD) % MOD
        let na = (ans + (ns * A.[num]) % MOD ) % MOD
        culc na ns (num + 1) ((e * 2L) % MOD)

(Ans + (culc 0L 0L 1 1L)) % MOD
|> printfn "%d"
