let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
let a = (1 + K) * K * N / 2
let b = (1 + N) * N * K * 100 / 2
printfn "%d" (a + b)

