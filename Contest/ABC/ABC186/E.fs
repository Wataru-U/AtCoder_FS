let t = stdin.ReadLine() |> int
let rec Gcd a b = if a % b = 0L then b else Gcd b (a%b)
Gcd 10000 
for i in 1 .. t do
    let [|N;S;K|] = stdin.ReadLine().Split() |> Array.map int64
    if (N-S) % K = 0L
        then (N-S) / K
        else 
            if N % K = 0L
                then -1L
                else 
                    let mutable d = K * ((N-S) / K)
                    if d = 0L then d <- 1L
                    while((d*K) % N = 0L) do
                        let amari = N + N % d
                        printfn "%d" (d%N)
                        d <- (d + amari) / K
                    d
    |> printfn "%d"

