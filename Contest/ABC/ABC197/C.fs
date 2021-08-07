let n = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |>Array.map int
let length = (pown 2 (n+1)) - 1

let rec culc bit num Or Xor prev = 
    let b = ((bit &&& (1 <<< num)) >>> num)
    if num = n then ((Or ^^^ Xor) |> int64)
    elif b <> prev then (culc bit (num+1) A.[num] (Or ^^^ Xor) b)
    else (culc bit (num+1) (A.[num] ||| Or) Xor b)

[|for i in 0..length -> (culc i 0 0 0 0)|]
|> Array.min
|> printfn "%d"


let n = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |>Array.map int

let rec culc bit num Or Xor prev = 
    let b = ((bit &&& (1 <<< num)) >>> num)
    if num = n then ((Or ^^^ Xor) |> int64)
    elif b <> prev then (culc bit (num+1) A.[num] (Or ^^^ Xor) b)
    else (culc bit (num+1) (A.[num] ||| Or) Xor b)

let mutable ans = 1000000000000L
for i in 0 .. ((pown 2 (n+1)) - 1) do
    ans <- min (culc i 0 0 0 0) ans
printfn "%d" ans