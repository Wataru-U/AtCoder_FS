let rec gcd a b = 
    match a % b with
    | 0 -> b
    | x -> gcd b (a%b) 


let [|N;X|] = stdin.ReadLine().Split() |> Array.map int
stdin.ReadLine().Split() 
|> Array.map int
|> Array.map (fun x -> x - X |> abs)
|> Array.sort
|> Array.rev
|> fun x ->
    if N = 1 then x.[0]
    else
        let mutable ans = x.[0]
        for i in 1..N-1 do
            ans <- gcd ans x.[i]
        ans
|> printfn "%d"