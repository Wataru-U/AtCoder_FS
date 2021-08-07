let [|A;B|] = stdin.ReadLine().Split() |> Array.map int
let a = A * 25 / 2 + (A%2)
let al = (A + 1) * 25 / 2 + ((A+1)%2)
let b = B * 10
let bl = (B+1) * 10
let ans = max a b
if ans >= (min al bl) then -1 else ans
|> printfn "%d"