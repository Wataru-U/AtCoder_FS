let n = stdin.ReadLine() |> float
let mutable ans = 0.
let mutable a = 1.
for i in 1..100 do
    let I = i |> float
    a <- a / n * (n-1.)
    ans <- ans + I * a