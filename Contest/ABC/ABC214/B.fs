let [|S;T|] = stdin.ReadLine().Split() |> Array.map int

let mutable ans = 0
for a in 0..S do
    for b in 0 .. (S - a) do
        for c in 0 .. (S - a - b) do
            if a * b * c <= T then ans <- ans + 1

printfn "%d" ans