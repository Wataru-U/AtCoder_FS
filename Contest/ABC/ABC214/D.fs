//WA
let N = stdin.ReadLine() |> int
let pass = [|for i in 1..N-1 -> stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)|]
let mutable To = [|for i in 1..N -> []|]

for i in pass do
    To.[i.[0]] <- (i.[1],(i.[2] + 1) |> int64) :: To.[i.[0]]
    To.[i.[1]] <- (i.[0],(i.[2] + 1) |> int64) :: To.[i.[1]]

