let [|N;X|] = stdin.ReadLine().Split() |> Array.map int
let mutable ans = -1
let mutable (sum:decimal) = 0.0m
let (o:decimal) = 100.0m
for i in 1 .. N do
    let [|V;P|] = stdin.ReadLine().Split() |> Array.map decimal
    sum <- sum + V * P / 100.0m
    if ans = -1 && sum > (X|>decimal) then ans <- i
printfn "%d" ans
