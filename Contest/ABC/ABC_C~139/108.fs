let [|N;K|] = stdin.ReadLine().Split() |> Array.map int64

N / K
|> fun x -> x * x * x
|> fun x ->
    if K % 2L = 0L
    then 
        (N / (K / 2L) - N / K)
        |> fun y -> x + y * y * y
    else x
|> printfn "%d"