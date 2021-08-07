stdin.ReadLine().Split() 
|> Array.map int
|> fun x -> x.[0] / x.[1]
|> printfn "%d"