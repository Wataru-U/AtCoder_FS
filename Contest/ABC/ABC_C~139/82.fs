stdin.ReadLine()
let calc (v:int64 * int) = 
    let x = (fst v, snd v |> int64)
    if fst x > snd x then snd x
    else fst x - snd x |> abs
stdin.ReadLine().Split()
|> Array.map int64
|> Array.countBy id
|> Array.map calc
|> Array.sum
|> printfn "%d"