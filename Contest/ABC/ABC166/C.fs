let [|H;M|] = stdin.ReadLine().Split() |> Array.map int
let tower = stdin.ReadLine().Split() |> Array.map int64
let mutable good = [|for i in 1 .. H -> true|]
for i in 1 .. M do
    let [|A;B|] = stdin.ReadLine().Split() |> Array.map (fun x -> (x|>int) - 1)
    if tower.[A] > tower.[B] then good.[B] <- false
    elif tower.[A] < tower.[B] then good.[A] <- false
    else 
        good.[B] <- false
        good.[A] <- false
good
|> Array.filter (fun x -> x)
|> fun x -> x.Length
|> printfn "%d"