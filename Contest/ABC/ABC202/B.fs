let change v = 
    match v with 
    | '6' -> '9'
    | '9' -> '6'
    | x -> x

stdin.ReadLine().ToCharArray()
|> Array.rev
|> Array.map change
|> fun x -> new string(x)
|> printfn "%s"