stdin.ReadLine().Split() 
|> Array.map int
|> Array.map (fun x -> 7 - x)
|> Array.sum 
|> printfn "%d"