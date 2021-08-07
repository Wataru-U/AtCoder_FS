let N = stdin.ReadLine() |> int
let v = stdin.ReadLine().Split() |> Array.map int64
let max (a:int64) (b:int64) = if a > b then a else b

let mutable dpi = Array.copy v //動作のi番目の中での最大値
let mutable sum = Array.copy v //i番目での和
let mutable p = Array.copy v //i番目までの動作の和

let mutable ans = max 0L v.[0]

for i in 1 ..N-1 do
    sum.[i] <- sum.[i] + sum.[i-1]
    p.[i] <- p.[i-1] + sum.[i]
    dpi.[i] <- max dpi.[i-1] sum.[i]
    ans <- max (ans) (p.[i-1]+dpi.[i])

ans |> printfn "%d" 