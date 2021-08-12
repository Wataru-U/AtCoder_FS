let N = stdin.ReadLine() |> int
let A = [|for i in 1..N -> stdin.ReadLine() |> int|]
let Sum = Array.sum A
if Sum % 10 <> 0 then
    printfn "%d" Sum
else
    A
    |> Array.filter (fun x -> x % 10 <> 0)
    |> fun x ->
        if x.Length = 0 then printfn "0"
        else
            x
            |> Array.sort
            |> Array.head
            |> fun x -> Sum - x
            |> printfn "%d"