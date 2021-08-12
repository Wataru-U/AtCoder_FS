let N = stdin.ReadLine() |> int
let mutable col = [|for i in 0..400..3200 -> 0|]
let player = stdin.ReadLine().Split() |> Array.map int

for i in player do
    let idx = min 8 (i / 400)
    col.[idx] <- col.[idx] + 1

let mutable ans = 0
for i in 0..7 do 
    if col.[i] <> 0 then ans <- ans + 1
let maxCol = ans + col.[8] 
if ans = 0 && col.[8] <> 0 then ans <- ans + 1
printfn "%d %d" ans maxCol