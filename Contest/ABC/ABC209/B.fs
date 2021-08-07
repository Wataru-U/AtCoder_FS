let [|N;X|] = stdin.ReadLine().Split() |> Array.map int
stdin.ReadLine().Split()
|> Array.map int
|> Array.sum
|> fun x ->
    if X >= x - N / 2 then "Yes" else "No"
|> printfn "%s"