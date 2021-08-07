open System

let [|A;B;H;M|] = stdin.ReadLine().Split() |> Array.map float
let h = (H + M / 60.0) / 6.0  * Math.PI
let m = M / 30.0 * Math.PI
let hx = A * (cos h)
let hy = A * (sin h)

let mx = B * (cos m)
let my = B * (sin m)

let x = hx - mx |> (fun x -> x * x)
let y = hy - my |> (fun x -> x * x)
x+y
|> sqrt
|> printfn "%.10f"