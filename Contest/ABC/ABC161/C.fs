let [|N;K|] = stdin.ReadLine().Split() |> Array.map int64
let ans = min (N%K) (abs (N%K-K))
printfn "%d" ans
