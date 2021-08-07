let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let A = stdin.ReadLine().Split() |> Array.map int
let MOD = M-1
let rec oneIdx index = 
    if A.[index] = 1 then index
    else oneIdx (index + 1)

let idx = oneIdx 0 + 1

let left = idx / M + if idx % MOD = 0 || idx = M then 0 else 1
let right = if idx = N then 0 else (N - (left * MOD + 1)) / MOD + if (N - (left * MOD + 1)) % MOD = 0  then 0 else 1

left + right
|> printfn "%d"