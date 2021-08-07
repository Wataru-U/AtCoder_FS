let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let edge = [|for i in 1..M -> stdin.ReadLine().Split() |> Array.map (fun x -> int(x) - 1)|]
let mutable col = [|for i in 1..N -> []|]
for i in edge do
    let idx = Array.max i
    let e = Array.min i
    col.[idx] <- e :: col.[idx]

let mutable pow3 = [|for i in 0..20 -> 1L|]
for i in 1..20 do
    pow3.[i] <- pow3.[i-1] * 3L

let mutable ans = pow3.[N]
let mutable No = 0L
for i in 0..N-1 do
    let mutable tyouhuku = [|for i in 1..N -> 0L|]
    let mutable Min = 3L
    for j in col.[i] do
        tyouhuku.[j] <- tyouhuku.[j] + 1L
        for k in col.[j] do
            tyouhuku.[k] <- tyouhuku.[k] + 1L
    Array.max tyouhuku
    |> fun x -> if x >= 2L then ans <- 0L
    if col.[i].Length <> 0 then
    No <- No + pow3.[col.[i].Length]
max (ans - No) 0L
|> printfn "%d" 