let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int64
let mutable sumArr = [|for i in 1..N -> 0L|]
sumArr.[0] <- A.[0]
for i in 1..N-1 do
    sumArr.[i] <- sumArr.[i-1] + A.[i]

let rec ans index m =
    if index = N - 1 then m
    else 
        let bear = sumArr.[index] - (sumArr.[N-1] - sumArr.[index]) |> abs
        ans (index + 1) (min m bear)
ans 0 10000000000000L
|> printfn "%d"