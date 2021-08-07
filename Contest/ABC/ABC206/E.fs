stdin.ReadLine()
|> float
|> fun x -> x * 1.08
|> fun x -> 
    if x > 207. then ":("
    elif x < 206. then "Yay!"
    else "so-so"
|> printfn "%s"

