let rec calc i v (a:int64 * int64) top =
    if i = top then a
    elif v % i = 0L then calc (i+1L) v (i,v/i) top
    else calc (i+1L) v a top

let N= stdin.ReadLine() |> int64

N
|> float
|> sqrt
|> int64
|> fun x -> x + 2L
|> fun x -> calc 1L N (0L,0L) x
|> fun x -> [|fst x;snd x|]
|> Array.max
|> string
|> Seq.length
|> printfn "%d"