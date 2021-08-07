let [|t;N|] = stdin.ReadLine().Split() |> Array.map float
let T = 100. / t
let ans = T * (N |> float) * (1. + t / 100.) 
if (ans |> int64 |> float) = ans then (ans |> int64) - 1L else ans |> int64
|> printfn  "%d"