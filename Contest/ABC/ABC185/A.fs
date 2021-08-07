stdin.ReadLine().Split()
|> Array.map int
|> Array.min
|> printfn "%d"