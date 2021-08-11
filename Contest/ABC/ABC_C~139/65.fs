let [|N;M|] = stdin.ReadLine().Split() |> Array.map int64
let MOD = 7L + (1e+9 |> int64)

let rec fac x = 
    match x with 
    | 1L -> 1L
    | _ -> x * fac(x-1L) % MOD

if abs (N - M) > 1L then 0L
elif abs (N - M) = 1L then fac N * fac M % MOD
else (fac N * fac M % MOD) * 2L % MOD
|> printfn "%d"