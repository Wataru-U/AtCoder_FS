let [|A;B;C|] = stdin.ReadLine().Split() |> Array.map int
A + B + C - (min A (min B C))
|> printfn "%d"