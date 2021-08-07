stdin.ReadLine().Split()
|> Array.map int
|> fun x -> (x.[0]+x.[1],x.[1])
|> fun (x,y) -> 
    if x >= 15 && y >= 8 then 1
    elif x >= 10 && y >= 3 then 2
    elif x >= 3 then 3
    else 4
|> printfn "%d"