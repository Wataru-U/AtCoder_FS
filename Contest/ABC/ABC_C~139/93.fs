let [|A;B;C|] = 
    stdin.ReadLine().Split() 
    |> Array.map int
    |> Array.sort
    |> Array.rev

let mutable ans = 0
2 * A - B - C
|> fun x -> 
    if x % 2 = 0 then x / 2
    else
        x / 2 + 2
|> printfn "%d"