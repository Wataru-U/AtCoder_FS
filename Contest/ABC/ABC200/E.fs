let mutable ans = 0L
let [|n;K|] = stdin.ReadLine().Split() |> Array.map int64
let N = n |> int
let per2 v = 
    let aMin = min(max (v - N) 1) 1
    let bMin = max (v - 2 * N) 1 
    let bMax = min (v - 2) N
    let aMax = min (v / 2) N
    let aMaxBmin = 2 * aMax + bMin
    let bMaxaMin = bMax + 2 * aMin
    printfn "%d %d %d %d %d %d" aMax bMax aMin bMin aMaxBmin bMaxaMin
 
per2 6
