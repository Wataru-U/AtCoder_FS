let N = stdin.ReadLine() |> int
let C = 
    stdin.ReadLine().Split()
    |> Array.map int64
    |> Array.sort


let MOD = 7L + (1e+9 |> int64)

let rec calc index v =
    if index = N then v
    else
        if C.[index] <= (index |> int64)then 0L
        else
            let nv = (v * (C.[index] - (index|> int64))) % MOD
            calc (index + 1) nv

calc 0 1L
|> printfn "%d"
