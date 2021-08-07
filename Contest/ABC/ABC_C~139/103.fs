let N = stdin.ReadLine() |> int
stdin.ReadLine().Split() 
|> Array.map int
|> Array.sum
|> fun x-> x - N
|> printfn "%d"