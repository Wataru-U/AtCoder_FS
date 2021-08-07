  
let MOD = 998244353L

let ModSum a = (((1L + a) * (a)) / 2L) % MOD

let [|A;B;C|] = stdin.ReadLine().Split() |> Array.map (fun x -> ModSum (x|> int64 |> (fun x -> x % MOD)))

((C * B) % MOD) * A % MOD |> printfn "%d"
