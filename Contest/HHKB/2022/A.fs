let x = stdin.ReadLine() |> int

let a = x / 100
let b = (x - a * 100) / 10
let c = x % 10

(a + b + c) * (111)
|> printfn "%d"