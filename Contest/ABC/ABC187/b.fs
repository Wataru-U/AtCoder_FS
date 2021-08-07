let n = stdin.ReadLine() |> int
let pos = [|for i in 1..n -> stdin.ReadLine().Split()|]
let tan a b = (a.[1] - b.[1]) / (a.[0] - b.[0])
let mutable count = 0
for i in 0..n-2 do
    for j in i+1..n-1 do
        if tan (x.[i]) (x[j]) >= -1 && tan (x.[i]) (x[j]) <= 1
            then count <- count + 1
count 
|> printfn "%d"