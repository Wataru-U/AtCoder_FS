let N = stdin.ReadLine() |> int
let A = [|for i in 1..N -> stdin.ReadLine() |> int|]
let top = Array.sort A |> Array.rev

for i in A do
    if i = top.[0] then top.[1] else top.[0]
    |> printfn "%d"