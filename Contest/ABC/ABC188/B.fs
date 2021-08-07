let n = stdin.ReadLine() |> int
[|for i in 0..1 -> stdin.ReadLine().Split() |> Array.map int|]
|> fun x ->
    let mutable ans = 0
    for i in 1 .. n do
        ans <- ans + x.[0].[i-1] * x.[1].[i-1]
    ans    
|> fun x -> if x = 0 then "Yes" else "No"
|> printfn "%s"