let[|S;P|] = stdin.ReadLine().Split() |> Array.map int64

[|1L .. 1000000L|]
|> Array.exists (fun x -> S - x = P / x && P % x = 0L)
|> function 
    | true -> "Yes"
    | false -> "No"
|> printfn "%s"

