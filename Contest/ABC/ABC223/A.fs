stdin.ReadLine()
|> int
|> fun x -> x % 100 = 0 && x <> 0
|> function 
    | true -> "Yes"
    | false -> "No"
|> printfn "%s"