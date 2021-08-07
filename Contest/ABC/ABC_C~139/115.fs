let [|N;M|] = stdin.ReadLine().Split() |> Array.map int

[|for i in 1..N -> stdin.ReadLine() |> int|]
|> Array.sort
|> fun x ->
    [|for i in 0..N-M -> x.[i + M - 1] - x.[i]|]
|> Array.min
|> printfn "%d"