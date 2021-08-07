stdin.ReadLine()
|> float
|> fun x -> x/3.0
|> fun x -> x * x * x
|> printfn "%.15f"