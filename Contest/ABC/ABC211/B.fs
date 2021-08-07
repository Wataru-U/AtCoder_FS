[|for i in 1..4 -> stdin.ReadLine()|]
|> Array.distinct
|> Array.length
|> (=) 4
|> function     
    | true -> "Yes"
    | false -> "No"
|> printfn "%s"