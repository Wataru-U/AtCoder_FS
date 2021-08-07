let N = stdin.ReadLine() |> int
stdin.ReadLine().Split()
|> Array.map int
|> Array.choose (fun x -> if x > 10 then Some(x-10) else None)
|> Array.sum
|> stdout.WriteLine