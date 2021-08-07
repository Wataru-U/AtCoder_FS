let [|x;y;z|] = stdin.ReadLine().Split() |> Array.map float
let v = y / x * z
if v = (v |> int |> float) then ((v |> int) - 1) else (v |> int)
|> printfn "%d"
