let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)
let B = stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)
let C = stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)

let mutable AN = [|for i in 1..N -> 0L|]
let mutable BN = [|for i in 1..N -> 0L|]
let mutable CN = [|for i in 1..N -> 0L|]

for i in C do
    CN.[i] <- 1L + CN.[i]

for i in 0..N-1 do
    BN.[B.[i]] <- BN.[B.[i]] + CN.[i]
    AN.[A.[i]] <- AN.[A.[i]] + 1L
let rec ans n v =
    if n = N then v
    else
        ans (n + 1) (v + AN.[n] * BN.[n])
ans 0 0L
|> stdout.WriteLine