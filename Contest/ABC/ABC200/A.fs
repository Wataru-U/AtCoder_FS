stdin.ReadLine()
|> int
|> fun x ->
    if x % 100 = 0 then x/100 else x/100+1
|> printfn "%d"