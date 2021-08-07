let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let mutable To = [|for i in 1..N -> []|]
let Add a b =
    To.[a] <- b :: To.[a]

for i in 1..M do
    let [|a;b|] = stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)
    Add a b

let mutable ans = 0

for i in 0..(N-1) do
    let mutable Seen = [|for j in 1 .. N -> false|]
    let rec dfs index= 
        if Seen.[index] then ()
        else 
            Seen.[index] <- true
            for j in To.[index] do
                dfs j
    dfs i
    Seen 
    |> Array.filter (fun x -> x)
    |> fun x -> (ans <- ans + x.Length)

ans
|> stdout.WriteLine 
