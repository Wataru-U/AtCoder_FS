open System

let [|o;s;theta|] = stdin.ReadLine().Split() |> Array.map float
let [|x;y;r|] = stdin.ReadLine().Split() |> Array.map float 

let rad2deg v = (180. / Math.PI) * v

let R = r / 2.
let g = 9.8

g * x * x
|> fun z -> z / (2. * s * s * (theta |> rad2deg |> Math.Cos |> (fun w -> w * w)))
|> fun z -> o + Math.Tan(rad2deg theta) * x - z
|> fun z -> (y - z) * 10.
|> Math.Floor
|> fun z -> z / 10.
|> abs
|> printfn "%.1f"
