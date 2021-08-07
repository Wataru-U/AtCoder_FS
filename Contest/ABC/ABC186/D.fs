let n = stdin.ReadLine() |> int
let v = stdin.ReadLine().Split() |> Array.map int64 |> Array.sort |> Array.rev
let mutable sum = v |> Array.sum
let mutable ans = 0L
for i in 0..n-2 do
    sum <- sum - v.[i]
    let m = n - 1 - i |> int64
    ans <- ans +  m * v.[i] - sum

ans |> printfn "%d"