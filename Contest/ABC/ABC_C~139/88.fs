[|
    for i in 1..3 ->
        stdin.ReadLine().Split()
        |> Array.map int
|]
|> fun x ->
    let a = 
        [|
            for i in 0..2 do
                [| 
                    for j in 0..1 ->
                        x.[i].[j] - x.[i].[j+1]
                |]
        |]
    let b = 
        [|
            for i in 0..2 do
                [| 
                    for j in 0..1 ->
                        x.[j].[i] - x.[j+1].[i]
                |]
        |]
    a.[0] = a.[1] && a.[1] = a.[2] && b.[0] = b.[1] && b.[1] = b.[2] 
|> function
    | true -> "Yes"
    | false -> "No"
|> printfn "%s"