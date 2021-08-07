stdin.ReadLine()
|> int
|> fun x -> 
    100 - (x % 100)
|> printfn "%d"