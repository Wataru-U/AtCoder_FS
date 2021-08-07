let N = stdin.ReadLine() |> int

[| for i in 1 .. N -> stdin.ReadLine().Split(' ') |> Array.map int64 |> (fun x -> (x.[0] + x.[1])*(x.[1] - x.[0] + 1L)/2L)|]
|> Array.sum |> printfn "%d"
