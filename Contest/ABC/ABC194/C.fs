let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int64
let mutable sum = Array.sum A
let mutable ans = 0L
for i in 0..N-2 do
    sum <- sum - A.[i]
    ans <- ans + 2L * A.[i] * sum
A 
|> Array.map (fun x -> x * x * (N-1|>int64))
|> Array.sum
|> fun x ->
    x - ans
|> printfn "%d"
