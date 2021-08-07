let N = stdin.ReadLine() |> int 
let mutable y = [| for i in 0 .. N-1 -> 0|]
let mutable x = [| for i in 0 .. N-1 -> 0|]
for i in 0 .. N-1 do
    let s = stdin.ReadLine().Split(' ') |> Array.map int|> Array.map (fun x -> x-1)
    x.[i] <- s.[0]
    y.[s.[0]] <- s.[1]

for i in 0 .. N-1 do 
    let mutable count =0
    if x.[i] <> 0  then 
        for j in 0 .. x.[i]-1 do
            if y.[j] < x.[i] then count <- count + 1
    if x.[i] <> N-1 then        
    for j in x.[i]-1 .. N-1 do
            if y.[j] > x.[i] then count <- count + 1
    printfn "%d" count        

