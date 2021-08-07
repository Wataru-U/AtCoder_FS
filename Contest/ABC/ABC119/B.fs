let n = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int |> Array.max
let B = stdin.ReadLine().Split() |> Array.map int |> Array.min
max (B-A+1) 0
|> printfn "%d"
