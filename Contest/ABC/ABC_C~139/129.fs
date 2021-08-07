let MOD = 7L + (1e+9 |> int64)

let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let A = [| for i in 1 .. M -> stdin.ReadLine() |> int |> (fun x -> x - 1) |]
let mutable ans = [|for i in 1 .. N -> 1L|]
let mutable stair = [|for i in 1 .. N -> false|]

for i in A do
    stair.[i] <- true


for i in 0..N-1 do
    if stair.[i] then ans.[i] <- 0L
    else
        if N >= 3 && i > 1
        then
            ans.[i] <- (ans.[i-1] + ans.[i-2]) % MOD
        elif i = 1 then
            ans.[i] <- ans.[i-1] + 1L

ans.[N-1] 
|> printfn "%d"

