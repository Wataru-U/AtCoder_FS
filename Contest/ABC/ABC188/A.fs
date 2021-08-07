stdin.ReadLine().Split()
|> Array.map int
|> fun x -> abs(x.[0] - x.[1])
|> fun x -> if x <3 then "Yes" else "No"
|> printfn "%s"