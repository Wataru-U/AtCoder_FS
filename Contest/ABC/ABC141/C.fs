let [|N;K;Q|] = stdin.ReadLine().Split() |> Array.map int

let mutable AC = [|for i in 1..N -> 0|]

[|for i in 1..Q ->stdin.ReadLine() |> int|]
|> Array.map int
|> Array.map (fun x -> AC.[x-1] <- AC.[x-1] + 1)

for i in AC do
    if K - Q + i> 0 then "Yes"
    else "No"
    |> printfn "%s"