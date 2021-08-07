let [|N;M|] = stdin.ReadLine().Split() |> Array.map int

let AC = [|for i in 1..N -> false|]
let p = [|for i in 1..N -> 0|]

for i in 1..M do 
    let q = stdin.ReadLine().Split()
    let index = (q.[0] |> int) - 1
    let v = q.[1]
    if v = "AC" then AC.[index] <- true
    else
        if not AC.[index] then p.[index] <- p.[index] + 1

let mutable ACcount = 0
let mutable pCount = 0
for i in 0..N-1 do
    if AC.[i] 
        then
            ACcount <- ACcount + 1
            pCount <- pCount + p.[i]

printfn "%d %d" ACcount pCount