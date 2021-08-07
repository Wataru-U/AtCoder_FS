let [|N;M;K|] = stdin.ReadLine().Split() |> Array.map int

let mutable map = 
    [|
        for i in 1..N ->
        [|
            for i in 1..N ->
                true
        |]
    |]

for i in 1..M do 
    stdin.ReadLine().Split()
    |> Array.map int
    |> Array.map (fun x -> x-1)
    |> fun x -> 
        map.[x.[0]].[x.[1]] <- false
        map.[x.[1]].[x.[0]] <- false

let lec calc i t = 
    if t = K then if map.[i].[0] = true then 1L else 0L
    else
        let mutable a = 0L 
        for i in 0..N-1 do
            