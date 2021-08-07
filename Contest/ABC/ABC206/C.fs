let N = stdin.ReadLine() |> int
let n = N |> int64
let mutable A = 
    stdin.ReadLine().Split() 
    |> Array.map int
    |> Array.sort

let mutable t = 0L
let mutable ans = 0L
for i in 2..N do
    let index = N - i
    if A.[index] <> A.[index + 1]
    then
        t <- 0L
    else
        t <- t + 1L
    ans <- ans + t
    
ans <- n * (n-1L) / 2L - ans
printfn "%d" ans


