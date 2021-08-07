let [|A;B;W|] = stdin.ReadLine().Split() |> Array.map int
let w = W * 1000
let a = w / A
let b = w / B
if a = b && w % A <> 0 then printfn "UNSATISFIABLE"
else
    let Min = a 
    let Max = if w % B = 0 then b else b+1
    printfn "%d %d" Max Min