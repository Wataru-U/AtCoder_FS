let N = stdin.ReadLine() |> int
let pos = [| for i in 1 .. N -> stdin.ReadLine().Split(' ') |> Array.map int |]
let mutable ans = 0
for i in 1..N-1 do
    ans <- ans + abs(pos.[i].[0] - pos.[i-1].[0]) + abs(pos.[i].[1] - pos.[i-1].[1])

printfn "%d" ans