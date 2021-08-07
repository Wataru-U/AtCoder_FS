let N = stdin.ReadLine() |> int64
let A = 
    [|for i in 0..4 -> stdin.ReadLine() |> int64|]
    |> Array.min 
if N % A = 0L then
    5L + N / A - 1L
else 
    5L + N / A
|> printfn "%d"
