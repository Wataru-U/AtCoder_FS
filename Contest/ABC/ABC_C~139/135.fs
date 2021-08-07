let N = stdin.ReadLine() |> int
let mutable A = stdin.ReadLine().Split() |> Array.map int64
let mutable B = stdin.ReadLine().Split() |> Array.map int64

let mutable ans = 0L

for i in 0..N-1 do
    ans <- ans + (min B.[i] A.[i])
    B.[i] <- max 0L (B.[i] - A.[i])
    ans <- ans + (min B.[i] A.[i+1])
    A.[i+1] <- max 0L (A.[i+1] - B.[i])

printfn "%d" ans