stdin.ReadLine()
|> int
|> fun x ->
    if x <= 125 then 4
    elif x <= 211 then 6
    else 8
|> printfn "%d"