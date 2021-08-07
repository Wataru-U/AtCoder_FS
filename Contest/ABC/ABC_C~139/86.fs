let calc (a : int[]) (b : int[]) = 
    [|
        for i in 0..2 do
            b.[i] - a.[i]
            |> abs
    |]
    |> fun x -> (x.[0],(x.[1] + x.[2]))
    |> fun (x,y) ->
        x % 2 = y % 2 && x >= y

let N = stdin.ReadLine() |> int

[|
    for i in 0..N -> 
        if i = 0 then [|0;0;0|]
        else stdin.ReadLine().Split() |> Array.map int
|]
|> fun x -> 
    [|
        for i in 1..N ->
            calc x.[i - 1] x.[i]
    |]
|> Array.filter (fun x -> not x)
|> Array.length
|> (=) 0
|> function
    | true -> "Yes"
    | false -> "No"
|> printfn "%s"