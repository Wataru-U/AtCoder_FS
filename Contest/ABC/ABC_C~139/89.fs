let N = stdin.ReadLine() |> int

[|
    for i in 1 .. N ->
        stdin.ReadLine() 
        |> fun x -> x.[0]
|]
|> Array.filter 
    (function 
        | 'M' | 'A' | 'R' | 'C' | 'H' -> true
        | _ -> false )
|> Array.countBy id
|> Array.map (snd >> int64)
|> fun x ->
    if x.Length < 3 then 0L
    else
        let mutable c = 0L
        for i in 0..x.Length-3 do
            for j in i+1 .. x.Length-2 do
                for k in j+1 .. x.Length-1 do
                    c <- c + x.[i] * x.[j] * x.[k]
        c
|> printfn "%d"