let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
let toInt c = 
    int(c) - int('a')
let str = stdin.ReadLine()
let mutable C = [|for i in 1..26 -> [|for i in 0..N -> N|]|]

for i in 0..(N-1) do
    let index = N - 1 - i
    for j in 0..25 do
        if (toInt str.[index]) = j then C.[j].[index] <- index
        else C.[j].[index] <- C.[j].[index+1]

let A = 'a' |> int
let makeStr j = 
    string(char(A + j))

let mutable cp = 0
let mutable ans = ""

let rec next i j =
    let np = C.[j].[cp]
    let m = N - np + i
    if m > K 
    then
        ans <- ans + "a"
        cp <- np + 1
    else
        next i (j+1)
for i in 1..K do
    next i 0


ans
|> printfn "%s"
