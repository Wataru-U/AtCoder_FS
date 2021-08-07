
stdin.ReadLine()
|> int
|> fun N -> [|for i in 1..N -> stdin.ReadLine().ToCharArray()|]
|> Array.map (fun x -> Array.sort x)
|> Array.countBy id
|> Array.map (snd >> fun x -> x |> int64)
|> Array.map (fun x -> (x-1L) * x / 2L)
|> Array.sum
|> printfn "%d"