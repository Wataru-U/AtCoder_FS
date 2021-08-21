let N = stdin.ReadLine() |> int64
let rec c v a =
    if v = N then a
    elif v > N then a-1
    else c (2L * v) (a+1)

c 1L 0
|> printfn "%d"