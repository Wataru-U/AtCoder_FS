let [|N;L|] = stdin.ReadLine().Split() |> Array.map int
let K = stdin.ReadLine()|> int
let A = stdin.ReadLine().Split() |> Array.map int
let diff = [|for i in 0..N-1 -> if i = 0 then A.[0] else A.[i] - A.[i-1]|]
let tail = L-A.[N-1]
let rec culc condition idx num sum = 
    if idx = N then   
        if (sum + tail >= condition && num = K) || num > K then true
        else 
            false
    elif sum + diff.[idx] >= condition then
        culc condition (idx+1) (num+1) 0
    else
        culc condition (idx+1) num (sum + diff.[idx])

let rec sarch left right =
    if right - left = 1 then
        if  culc right 0 0 0 then right
        else left
    elif culc ((left + right) / 2) 0 0 0 then
        sarch ((left + right) / 2) right 
    else 
        sarch left ((left + right) / 2)

sarch 0 L
|> printfn "%d"