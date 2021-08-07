let [|a;b;c;d|] = stdin.ReadLine().Split(' ') |> Array.map int64
let Max (a:int64) (b:int64) = if a > b then a else b
Max (a*c) (a * d) |> Max (b * c) |> Max (b * d) |> printf "%d"