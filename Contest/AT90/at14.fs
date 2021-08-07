let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int64 |> Array.sort
let B = stdin.ReadLine().Split() |> Array.map int64 |> Array.sort

A
|> Array.mapi (fun i x -> abs (x-B.[i]))
|> Array.sum 
|> stdout.WriteLine