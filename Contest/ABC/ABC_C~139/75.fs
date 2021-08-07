let [|N;M|] = stdin.ReadLine().Split() |> Array.map int

let mutable To = [|for i in 1..N -> []|]
let pass = [|for i in 1..M -> stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)|]
for i in pass do
    let [|x;y|] = i
    To.[x] <- y :: To.[x]
    To.[y] <- x :: To.[y]

let rec bfs index (arr: byref<int[]>) (dist: byref<int[]>) (tail: byref<int>) x y =
    if index = N || tail = N then false
    elif arr.[index] = -1 then true
    else
        do 
            for i in 0..To.[arr.[index]].Length-1 do
                let t = To.[arr.[index]].[i]
                if not ((arr.[index] = x && t = y) || (arr.[index] = y && t = x))
                then
                    if dist.[t] = -1 
                    then
                        dist.[t] <- 1
                        arr.[tail] <- t
                        tail <- tail + 1
        bfs (index + 1) &arr &dist &tail x y

let mutable ans = 0

for i in pass do
    let mutable dist = [|for i in 1..N -> -1|]
    let mutable arr = [|for i in 1..N -> -1|]
    let mutable tail = 1
    arr.[0] <- 0
    dist.[0] <- 0
    if bfs 0 &arr &dist &tail i.[0] i.[1]
    then
        ans <- ans + 1

printfn "%d" ans