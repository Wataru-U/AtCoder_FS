stdin.ReadLine().Split()
|> Array.map int
|> fun x ->
    if x.[0] = 0 then "Silver"
    elif x.[1] = 0 then "Gold"
    else "Alloy"
|> printfn "%s"