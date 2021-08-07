let N = stdin.ReadLine() |> int
stdin.ReadLine().Split()
|> Array.map int64
|> Array.countBy id
|> Array.filter (snd >> fun x -> x > 1)
|> Array.sortBy fst
|> Array.rev
|> fun x -> 
    if x.Length = 0 || (x.Length = 1 && snd x.[0] < 4) then 0L
    elif snd x.[0] > 3 then fst x.[0] |> (fun y -> y * y)
    else fst x.[1] * fst x.[0]
|> printfn "%d"