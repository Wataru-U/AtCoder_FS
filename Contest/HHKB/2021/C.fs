let N = stdin.ReadLine() |> int
let mutable b = [|for i in 0 .. 200000 -> false|]
let mutable min = 0

let rec updateMin () =
    if b.[min] = true then 
        min <- min + 1
        updateMin ()
    else ()

let P = stdin.ReadLine().Split(' ') |> Array.map int
for i in 0 .. N-1 do
    b.[P.[i]] <- true
    if min = P.[i] then updateMin()
    printfn "%d" min