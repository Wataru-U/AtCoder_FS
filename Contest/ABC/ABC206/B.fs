let N = stdin.ReadLine() |> int
let rec calc x v = 
    if v >= N then x
    else calc (x+1) (v + x + 1)

calc 1 1
|> printfn "%d"

   