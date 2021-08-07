let N = stdin.ReadLine() |> int
let A =
    [|
        for i in 1..2 -> 
            stdin.ReadLine().Split() 
            |> Array.map int
    |]

let mutable Sum = Array.map (Array.sum) A
Sum.[0] <- A.[0].[0]

let a = Array.sum Sum
if N = 1 then a
else
    seq {
        for i in 1..N-1 do
            Sum.[0] <- Sum.[0] + A.[0].[i]
            Sum.[1] <- Sum.[1] - A.[1].[i-1]
            Array.sum Sum
    }
    |> Seq.max
    |> max a
|> printfn "%A"