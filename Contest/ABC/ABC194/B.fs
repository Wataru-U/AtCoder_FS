let n = stdin.ReadLine() |> int
let A = [|for i in 1 .. n -> stdin.ReadLine().Split() |> Array.map int|]
let mutable Min = 10000000
for i in 0..n-1 do
    for j in 0..n-1 do
        if i = j 
        then Min <- min Min (A.[i].[0] +  A.[i].[1])
        else  Min <- min Min (max A.[i].[0]  A.[j].[1])
printfn "%d" Min