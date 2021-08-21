stdin.ReadLine()
|> (=) "Hello,World!"
|> function 
    | true -> "AC"
    | _ -> "WA"
|> printfn "%s"