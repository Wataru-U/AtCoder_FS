//WA
let N = stdin.ReadLine() |> int
let pass = 
    [|for i in 1..N-1 -> stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)|]
    |> Array.sortBy (fun x -> x.[2])
let mutable Parent = [|for i in 0..N-1 -> i|]
let mutable cnt = [|for i in 0..N-1 -> 0L|]
let rec root index = 
    if index = Parent.[index] then index
    else root Parent.[index]

let mutable ans = 0L

for i in pass do
    let pa = min (root i.[0]) (root i.[1])
    let pb = max (root i.[0]) (root i.[1])
    Parent.[pb] <- pa
    cnt.[pa] <- cnt.[pa] + cnt.[pb] + 1L
    ans <- ans + ((i.[2] + 1) |> int64) * cnt.[pa]

printfn "%d" ans

