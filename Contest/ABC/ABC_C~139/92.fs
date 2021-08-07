let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int

let diff = 
    [|
        for i in 0..N-1 ->
            match i with
            | 0 -> A.[i]
            | _ -> A.[i - 1] - A.[i]
            |> abs
    |]

let diff2 = 
    [|
        for i in 1..N-1 ->
            match i with 
            | 1 -> A.[i]
            | _ -> A.[i-2] - A.[i]
            |> abs
    |]

let Sum = 
    diff 
    |> Array.sum
    |> fun x -> x + abs (Array.last A)

let rec calc index =
    if index = N-1 then
        Sum - diff.[index] - (abs A.[index]) + (abs A.[index - 1])
        |> printfn "%d"
    else
        Sum - diff.[index] - diff.[index + 1] + diff2.[index]
        |> printfn "%d"
        calc (index + 1)

calc 0         
