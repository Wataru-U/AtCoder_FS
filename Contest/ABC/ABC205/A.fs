let [|A;B|] = stdin.ReadLine().Split() |> Array.map float
A * B / 100.
|> printfn "%15f"