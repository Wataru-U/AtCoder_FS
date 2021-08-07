let n = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split() |> Array.map int64
let b = stdin.ReadLine().Split() |> Array.map int64

let mutable ans = a.[0] * b.[0]
let mutable aMax =  a.[0]
let mutable bMax =  b.[0]
ans |> printfn "%d"

for i in 1..n-1 do
    aMax <- max a.[i] aMax
    ans <- max ans (b.[i] * aMax)
    ans |> printfn "%d"