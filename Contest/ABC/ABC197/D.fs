open System;

let n = stdin.ReadLine() |> int
let [|x_0;y_0|] = stdin.ReadLine().Split() |> Array.map float
let [|x_n;y_n|] = stdin.ReadLine().Split() |> Array.map float

let theta = Math.PI / ((n / 2) |> float)

let xDiff = x_n - x_0 |> (fun x -> x * x)
let yDiff = y_n - x_0 |> (fun x -> x * x)
let dis = xDiff + yDiff |> sqrt

let xcenter = (x_n + x_0) / 2.
let ycenter = (y_n + y_0) / 2.

let x = (x_0 - xcenter)
let y = (y_0 - ycenter)

let cosx = cos theta
let sinx = sin theta

let ansx = (x * cosx - y * sinx) + xcenter
let ansy = (x * sinx + y * cosx) + ycenter

printfn "%s %s" (ansx|>string) (ansy|>string)