let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let road = [|for i in 1..M -> stdin.ReadLine().Split() |> Array.map int|]

let mutable map = 
    [|
        for i in 1..N -> 
            [|
                for j in 1..N -> -1L
            |]
    |]

let mutable from = 
    [|
        for i in 0..N-1 ->
            []
    |]

let dp = 
    [|
        for i in 1..N -> 
            [|
                for j in 1..N -> 
                    [|
                        for k in 1..N -> -1L
                    |]
            |]
    |]
let mutable ans = 0L


for x in road do
    map.[x.[0] - 1].[x.[1] - 1] <- x.[2] |> int64
    from.[x.[1] - 1] <- x.[0] - 1


let Min a b = 
    if a < 0L && b < 0L then -1L
    elif a < 0L || b < 0L then max a b
    else min a b

for k in 1..N-1 do
    for t in 0..N-1 do
        for s in from.[t] do  
            dp.[k].[s].[t] <- Min dp.[k].[s].[t]

printfn "%d" ans