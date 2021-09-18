stdin.ReadLine()
|> int
|> fun x -> 
    if x < 40 
    then
        40 - x
        |> string
    elif x < 70 
    then
        70 - x
        |> string
    elif x < 90
    then 
        90 - x
        |> string
    else "expert"
|> printfn "%s"