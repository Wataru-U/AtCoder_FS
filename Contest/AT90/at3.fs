let n = stdin.ReadLine() |> int
let mutable seen = [|for i in 1..n -> -1|]
let mutable root = [|for i in 1..n -> []|]

let In a b = 
    root.[a] <- b :: root.[a]
    root.[b] <- a :: root.[b]

for i in 2..n do
    stdin.ReadLine().Split()
    |> Array.map int
    |> Array.map (fun x -> x-1)
    |> fun x -> In x.[0] x.[1]


let rec bfs index = 
    for i in root.[index] do
        if seen.[i] = -1 
        then 
            seen.[i] <- seen.[index] + 1
            bfs i 
        else ()

seen.[0] <- 0
bfs 0
let index =
    let mutable idx = 0
    let mutable Max = 0
    for i in 0..(n-1) do
        if Max < seen.[i] then
            Max <- seen.[i]
            idx <- i
    idx
seen <- [|for i in 1..n -> -1|]
seen.[index] <- 1
bfs index
seen
|> Array.max
|> printfn "%d"



