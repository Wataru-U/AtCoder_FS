let [|N;M|] = stdin.ReadLine().Split() |> Array.map int64

if N = 1L && M = 1L then 1L
elif N = 1L || M = 1L then (max N M) - 2L
else
    N * M
    |> fun x -> x - 2L * (N + M - 2L)
|> printfn "%d"