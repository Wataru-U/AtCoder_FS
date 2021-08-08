let [|A;B|] = stdin.ReadLine().Split() |> Array.map int
A ^^^ B
|> printfn "%d"