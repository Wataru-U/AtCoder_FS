stdin.ReadLine()
stdin.ReadLine().Split()
|> Array.countBy id
|> Array.filter (snd >> fun x -> x <> 1)
|> Array.length
|> function
    | 0 -> "YES"
    | _ -> "NO"
|> printfn "%s"
