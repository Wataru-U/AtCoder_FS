let [|N;K|] = stdin.ReadLine().Split() |> Array.map int 
stdin.ReadLine().Split()
|> Array.map int
|> Array.countBy id
|> Array.sortBy (fun (_,x) -> x)
|> fun x -> 
    let mutable ans = 0L
    for i in 0.. x.Length-K-1 do
        ans <- ans + (snd x.[i] |> int64)
    ans
|> printfn "%d"