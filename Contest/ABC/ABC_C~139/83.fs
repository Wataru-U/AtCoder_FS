stdin.ReadLine().Split()
|> Array.map int64
|> fun x -> 
    let mutable c = 0L
    let mutable a = x.[0]
    while a <= x.[1] do
        a <- a * 2L
        c <- c + 1L
    c
|> printfn "%d"