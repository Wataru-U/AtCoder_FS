let mutable N = stdin.ReadLine() |> int64

let mutable [|t;f|] = [|1L;1L|]

let mutable b = true
for i in 1 .. 37 do
    t <- t * 3L
    f <- 1L
    for j in 1 .. 25 do
        f <- f * 5L
        if t + f = N then printfn "%d %d" i j; b <- false

if b then printfn "-1"

-8 % 6 % 6