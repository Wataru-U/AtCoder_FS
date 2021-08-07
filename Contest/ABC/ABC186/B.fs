let [|H;W|] = stdin.ReadLine().Split() |> Array.map int
let mutable sum = 0
let mutable m = 101
for i in 1 .. H do
    let v = stdin.ReadLine().Split() |> Array.map int
    sum <- sum + Array.sum v
    m <- min m (Array.min v)

sum - m * H * W
|> printfn "%d"