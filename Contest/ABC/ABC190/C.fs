let [|N;M;|] = stdin.ReadLine().Split() |> Array.map int
let mutable v = [|for i in 1..N -> false|]
let q = [|for i in 1 .. M -> stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1) |]
let count () = 
    let mutable c = 0
    for i in q do
        if v.[i.[0]] && v.[i.[1]] then c <- c + 1
    c

let k = stdin.ReadLine() |> int
let p = [|for i in 1 .. k -> stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1) |]
let mutable num = 1
for i in 0..(k-1) do
    num <- num * 2
let mutable ans = -1
for i in 0..(num - 1) do
    v <- [|for i in 1..N -> false|]
    for j in 0..(k-1) do
        if (i &&& (1 <<< j)) >>> j = 1 then v.[p.[j].[0]] <- true else v.[p.[j].[1]] <- true
    ans <- max (count ())  ans

ans |> printfn "%d"