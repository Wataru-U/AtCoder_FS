stdin.ReadLine().Split()
|> Array.map float
|> fun x ->
    (x.[0] - x.[1]) / 3. + x.[1]
|> printfn "%.15f"