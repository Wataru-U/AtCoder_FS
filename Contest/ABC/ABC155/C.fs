let N = stdin.ReadLine() |> int
let S = [|for i in 1..N -> stdin.ReadLine()|] |> Array.sort |> Array.countBy id

S
|> Array.maxBy (snd >> fun x -> x)
|> snd
|> fun key -> Array.filter (snd >> fun x -> x = key) S
|> fun x -> 
    for ans in x do
        printfn "%s" (fst ans)
