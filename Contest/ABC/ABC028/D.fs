let [|N;K|] = stdin.ReadLine().Split() |> Array.map float


let ue = (N-K) * (N-K) * 3.
let sita = (K-1.) * (K-1.) * 3.
let denai = (N-1.) * (N-1.) * (N-1.)

1. - (denai + ue + sita) / (N * N * N)
|> printfn "%.15f"