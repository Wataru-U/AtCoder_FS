let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let mutable one = [|for i in 1..N -> false|]
let mutable toN = [|for i in 1..N -> false|]

for i in 1..M do
    let [|a;b|] = stdin.ReadLine().Split() |> Array.map int
    if a = 1 || b = 1 then one.[(max a b)-1] <- true
    if a = N || b = N then toN.[(min a b)-1] <- true

let rec ans index = 
    if index = N-1 then "IMPOSSIBLE"
    elif one.[index] && toN.[index] then "POSSIBLE"
    else ans (index + 1)
ans 1
|> printfn "%s"
