stdin.ReadLine()
stdin.ReadLine().Split() 
|> Array.map int
|> fun x ->
    let mutable min = 1000000
    let mutable ans = 0
    for i in x do
        if i < min then 
            ans <- ans + 1
            min <- i
    ans
|> printfn "%d"