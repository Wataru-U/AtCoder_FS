let [|N;M|] = stdin.ReadLine().Split() |> Array.map int

stdin.ReadLine().Split() 
|> Array.map int
|> Array.sort
|> fun x ->  [|for i in 1..M-1 -> x.[i] - x.[i-1]|]
|> Array.sort 
|> Array.rev
|> fun x -> 
    if N >= M then 0
    else
        x.[N-1..]
        |> Array.sum
|> printfn "%d"
