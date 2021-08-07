let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
(1900 * M + (N-M) * 100) * (pown 2 M)
|> printfn "%d"