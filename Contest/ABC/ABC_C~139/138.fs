let N = stdin.ReadLine() |> int
let A = 
    stdin.ReadLine().Split()
    |> Array.map float
    |> Array.sort

let rec calc index v = 
    if index = N then v 
    else
        calc (index + 1) ((v + A.[index]) / 2.)

calc 1 (A.[0])
|> printfn "%15f"