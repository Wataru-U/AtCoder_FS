stdin.ReadLine().ToCharArray()
|> Array.map string
|> fun x -> x.[0] = x.[1] && x.[1] = x.[2]
|> function
| true -> "Won"
| _ -> "Lost"
|> printfn "%s"
