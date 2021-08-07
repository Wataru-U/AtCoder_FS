let [|N;K|] = stdin.ReadLine().Split() |> Array.map int

let rec log2 x v=
    if x >= v then x
    else log2 (x * 2.) v

let omote v = 
    log2 1. v


[|for i in 1..N -> 
    (K |> float) / (i |> float)
    |> omote
    |> fun x -> 1. / x
|]
|> Array.sum
|> fun x -> x / (N |> float)
|> printfn "%.15f"