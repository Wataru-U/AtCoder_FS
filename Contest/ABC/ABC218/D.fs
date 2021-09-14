let N = stdin.ReadLine() |> int
let pos = 
    [|
        for i in 1..N -> 
            stdin.ReadLine().Split()
    |]

let same_x = [|for i in 1..N-1 -> []|]
let same_y = [|for i in 1..N-1 -> []|]

for i in 0..N-2 do
    for j in (i+1) .. (N-1) do
        if pos.[i].[0] = pos.[j].[0]
        then same_x.[i] <- j :: same_x.[i]

        if pos.[i].[1] = pos.[j].[1]
        then same_y.[i] <- j :: same_y.[i]

let mutable ans = 0
        
for i in 0..N-2 do
    if same_x.[i].Length > 0 && same_y.[i].Length > 0 
    then
        for j in same_x.[i] do
            if j <> N-1
            then
                for j_ in same_y.[j] do
                    for k in same_y.[i] do
                        if k <> N-1
                        then
                            for k_ in same_x.[k] do
                                    if j_ = k_ then ans <- ans + 1
                        else
                            for k_ in same_x.[j_] do
                                if k_ = N-1 then ans <- ans + 1


            else
                for k in same_y.[i] do
                    if k < N-1
                    then
                        for k_ in same_x.[k] do
                            if k_ <> N-1
                            then
                                for l in same_y.[k_] do 
                                    if l =  N-1 then ans <- ans + 1
        if 

printfn "%d" ans