let n = stdin.ReadLine() |> int
let mutable ans = -1L
for i in 1 .. n do
    stdin.ReadLine().Split()
    |> Array.map int64
    |> fun x ->
        if x.[2] - x.[0] > 0L && (ans > x.[1] || ans = -1L)
            then ans <- x.[1]
printfn "%d" ans