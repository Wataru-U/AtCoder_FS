let mutable [|N;M;T|] = stdin.ReadLine().Split() |> Array.map int64
let mutable o = 0L
let mutable l = N
let mutable b = true
for i in 1 .. M |> int do
    let [|A;B|] = stdin.ReadLine().Split() |> Array.map int64
    l <- l - A + o
    if l < 1L && b
        then 
            b <- false
    l <- l + B - A
    l <- min l N
    o <- B
if b && l > T - o then printfn "Yes" else printfn "No"