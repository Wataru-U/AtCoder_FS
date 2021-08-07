stdin.ReadLine().Split()
|> Array.map float
|> fun x -> (x.[0] - x.[1]) / x.[0] * 100.0
|> string
|> printfn "%s" 