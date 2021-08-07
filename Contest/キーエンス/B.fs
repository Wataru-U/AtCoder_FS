let [|N;K|] = stdin.ReadLine().Split() |> Array.map int
let v = stdin.ReadLine().Split() |> Array.map int
let mutable num = [|for i in 1 .. N -> 0|]

for item in v do
    num.[item] <- num.[item] + 1

let mutable ans = 0
let mutable n = num.[0]
let mutable b = true
for i in 1..N-1 do
    if b && num.[i] <= K && n > num.[i]
        then 
            ans <- ans + i * ((min K n) - num.[i])
            n <- num.[i]
            if num.[i] = 0
                then b <- false

if b && n > 0
    then ans <- ans + N * (n) 

ans 
|> printfn "%d"