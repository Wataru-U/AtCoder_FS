let [|a;b|] = stdin.ReadLine().Split() |> Array.map int
let [|c;d|] = stdin.ReadLine().Split() |> Array.map int
b - c 
|> printfn "%d"