let [|N;Q|] = stdin.ReadLine().Split() |> Array.map int
let (To: int list []) = 
    Array.init N (fun _ -> [])
    |> fun x -> 
        for i in 2..N do
            stdin.ReadLine().Split()
            |> Array.map int
            |> Array.map (fun y -> y - 1)
            |> fun y ->
                x.[y.[0]] <- List.append x.[y.[0]] [y.[1]]
                x.[y.[1]] <- List.append x.[y.[1]] [y.[0]]
        x

let query = 
    [|
        for i in 1..Q -> 
            stdin.ReadLine().Split() 
            |> Array.map int 
            |> Array.map (fun x -> x-1)
    |]

let rank = Array.init N (fun _ -> -1)


let rec bfs index r =
    for i in To.[index] do
        do
            if rank.[i] = -1
            then
                Array.set rank i r
                bfs i (r+1)


rank.[0] <- 0
bfs 0 1

for i in query do
    if (rank.[i.[0]] + rank.[i.[1]]) % 2 = 0 then "Town" else "Road"
    |> printfn "%s"

