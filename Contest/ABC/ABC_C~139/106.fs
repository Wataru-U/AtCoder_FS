let S = stdin.ReadLine()
let K = stdin.ReadLine() |> int64

let rec calc index = 
    if (index |> int64) = K - 1L && S.[index] = '1' then "1"
    else
        if S.[index] = '1' then calc (index + 1)
        else S.[index] |> string

calc 0
|> printfn "%s"
