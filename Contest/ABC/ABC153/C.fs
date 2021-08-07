let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
if N <= K then 0L
else
    stdin.ReadLine().Split() 
    |> Array.map int64
    |> Array.sort
    |> Array.rev
    |> fun x -> x.[K..]
    |> Array.sum
|> printfn "%d"