let [|K;N|] = stdin.ReadLine().Split() |> Array.map int
let A = stdin.ReadLine().Split() |> Array.map int
let start = A.[0] + K - A.[N-1]
[|for i in 0..N-2 -> A.[i+1] - A.[i]|]
|> Array.max
|> fun x ->
    K - (max x start)
|> printfn "%d"