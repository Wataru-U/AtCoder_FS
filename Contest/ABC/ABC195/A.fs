stdin.ReadLine().Split()
|> Array.map int
|> fun x ->
    match (x.[1] % x.[0]) with
    | 0 -> "Yes"
    | _ -> "No"
|> printfn "%s"