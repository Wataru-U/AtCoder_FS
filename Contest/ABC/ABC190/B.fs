let n = stdin.ReadLine() |> int64
let mutable ans = 0

let mutable a = 0L
for i in 1 .. 2000000 do
    let b = i |> int64
    a <- a + b
    if (n-a) % b = 0L && b * (b+1L) / 2L <= n then ans <- ans + 2

printfn "%d" ans

