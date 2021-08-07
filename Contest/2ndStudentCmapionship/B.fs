let [|n;m|] = stdin.ReadLine().Split() |> Array.map int
let A = stdin.ReadLine().Split() |> Array.map int
let B = stdin.ReadLine().Split() |> Array.map int
let mutable b = [|for i in 1..m -> false|]

let mutable (ans:list<int>) = []
for i in A do
    let mutable a = false
    for j in 0..(m-1) do
        if i = B.[j] then
            a <- true
            b.[j] <- true
    if a = false then ans <- i :: ans
for i in 0..m-1 do
    if b.[i] = false then ans <- B.[i] :: ans

ans 
|> List.sort
|> fun x ->
    for i in x do
        printfn "%d" i
