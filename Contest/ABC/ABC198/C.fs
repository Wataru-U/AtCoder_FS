let [|R;X;Y|] = stdin.ReadLine().Split() |> Array.map float
let x = X / 10.
let y = Y / 10.
let diff = x * x + y * y |> sqrt |> (fun x -> x * 10.)
let rec ans x n = 
    if x >= diff then n else ans (x + R) (n+1)
printfn "%d" (if diff < R then 2 else (ans R 1))