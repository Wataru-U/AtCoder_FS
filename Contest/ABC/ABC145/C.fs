let dist (a:int[]) (b:int[]) = 
    let x = a.[0] - b.[0] |> (fun x -> x * x)
    let y = a.[1] - b.[1] |> (fun x -> x * x)
    x + y
    |> float
    |> sqrt

let N = stdin.ReadLine() |> int

let pos = [| for i in 1..N -> stdin.ReadLine().Split() |> Array.map int|]


[| for i in pos ->
    [| for j in pos ->
        dist i j
    |]
|]
|> Array.map (fun x -> Array.sum x)
|> Array.sum 
|> fun x -> x / ((N |> float))
|> printfn "%15f"