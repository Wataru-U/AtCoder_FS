let N = stdin.ReadLine()
stdin.ReadLine().Split()
|> Array.map int
|> Array.mapi (fun i x -> (x,i+1))
|> Array.sortBy fst
|> Array.rev
|> fun x -> x.[1]
|> snd
|> printfn "%d"