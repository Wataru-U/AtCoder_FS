let n = stdin.ReadLine() |> int
let mutable nokori = [|for i in 1 .. 9 -> n|]
let t = stdin.ReadLine().ToCharArray() |> (fun x -> x.[0..3]) |> Array.map (string) |> Array.map int
let a = stdin.ReadLine().ToCharArray() |> (fun x -> x.[0..3]) |> Array.map (string) |> Array.map int
let mutable tp = [|for i in 1..9 -> 0|]
let mutable ap = [|for i in 1..9 -> 0|]
let N = (9 * n - 9) * (9 * n - 10) |> float
for i in 0..3 do
    nokori.[t.[i]-1] <-  nokori.[t.[i]-1] - 1
    nokori.[a.[i]-1] <-  nokori.[a.[i]-1] - 1
    tp.[t.[i]-1] <- tp.[t.[i]-1] + 1
    ap.[a.[i]-1] <- ap.[a.[i]-1] + 1
let pow n = 
    let mutable r = 1
    for i in 1 .. n do
        r <- r * 10
    r
let point (valu: int[]) = 
    let mutable v = 0
    for i in 0..8 do
        v <- v +  (i+1) * (pow valu.[i])
    v
    |>float
let mutable kakuritu = 0.0
for i in 0 .. 8 do
    if nokori.[i] > 0
    then
        for j in 0 .. 8 do
            if nokori.[i] > 1 || (nokori.[i] = 0 && i <> j)
                then
                    let tt = [|for k in 0..8 -> if k = i then tp.[k] + 1 else tp.[k]|]
                    let aa = [|for k in 0..8 -> if k = j then ap.[k] + 1 else ap.[k]|]
                    if (point tt) > (point aa)
                        then kakuritu <- kakuritu + ((nokori.[i]) |> float) * (nokori.[j] |> float) / N

kakuritu 
|> string
|> printfn "%s"
