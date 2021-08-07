let N = stdin.ReadLine() |> int
let v = stdin.ReadLine().Split() |> Array.map int

let mutable a = [|for i in 0..100000 -> 0|]
let mutable b = [|for i in 0..100000 -> 0|]

for i in 0..N-1 do  
    if i % 2 = 0
    then
        a.[v.[i]] <- a.[v.[i]] + 1
    else
        b.[v.[i]] <- b.[v.[i]] + 1

let ans_1 = 
    a
    |> Array.mapi (fun i x -> (x,i))
    |> Array.sortBy (fst >> fun x -> x)
    |> Array.rev

let ans_2 = 
    b
    |> Array.mapi (fun i x -> (x,i))
    |> Array.sortBy (fst >> fun x -> x)
    |> Array.rev


if snd ans_1.[0] <> snd ans_2.[0]
    then
    N - fst ans_1.[0] - fst ans_2.[0]
    else
    N - fst ans_1.[0] - fst ans_2.[1]
    |> min (N - fst ans_1.[1] - fst ans_2.[0])
|> printfn "%d"
    