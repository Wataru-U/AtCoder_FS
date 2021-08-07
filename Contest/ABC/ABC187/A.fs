stdin.ReadLine().ToCharArray()
|> Array.filter (fun x -> x <> ' ')
|> Array.map (fun x -> x.ToString() |> int)
|> fun x -> max (Array.sum x.[0..2]) (Array.sum x.[3..5])
|> printfn "%d"
