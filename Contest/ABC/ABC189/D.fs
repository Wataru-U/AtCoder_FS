let mutable t = 1L
let mutable f = 1L
let n = stdin.ReadLine() |> int
for i in 1 .. n do
    stdin.ReadLine()
    |> function
    | "OR" -> t <- 2L * t + f
    | _ -> f <- 2L * f + t
printfn "%d" t
