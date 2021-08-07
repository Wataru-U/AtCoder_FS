stdin.ReadLine().Split() 
|> Array.map int
|> fun x -> 
    let mutable a = -1
    let mutable b = -1
    let mutable c = -1
    for i in 0..x.[0] do
        for j in 0..x.[0]-i do
            let k = x.[0] - i - j
            if 1000 * i + 5000 * j + 10000 * k = x.[1]
            then 
                a <- k
                b <- j
                c <- i
    (a,b,c)
|> fun (a,b,c) ->
    printfn "%d %d %d" a b c


