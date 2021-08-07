let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int
let mutable num = [|for i in 0..N -> 0L|]
for i in 0 .. N-1 do
    num.[A.[i]] <- num.[A.[i]] + 1L
let Max = 
    num
    |> Array.map (fun x -> x * (x-1L) / 2L) 
    |> Array.sum 
for i in 0 .. N-1 do
    (num.[A.[i]] - 1L)
    |> fun x -> Max - x
    |> printfn "%d"
