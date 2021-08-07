stdin.ReadLine().ToCharArray()
|> Array.map (string >> int)
|> fun x ->
    let mutable b = true
    for i in 0..7 do
        let mutable k = x.[0]
        let mutable ans = x.[0] |> string
        for j in 0..2 do
            i >>> j &&& 1
            |> (=) 1
            |> function
                | true ->
                    k <- k + x.[j+1]
                    ans <- ans + "+"
                | false -> 
                    k <- k - x.[j+1]
                    ans <- ans + "-"
            ans <- ans + (x.[j+1] |> string)
        if k = 7 && b then  
            printfn "%s=7" ans
            b <- false