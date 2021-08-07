let N = stdin.ReadLine() |> int
let A = [|for i in 1..N -> stdin.ReadLine().Split() |> Array.map int|]
let B = [|for i in 1..N -> stdin.ReadLine().Split() |> Array.map int|]


let ToPos (x:int[][]) = 
    let mutable Array = [|for i in 0..40 -> [|for j in 0..40 -> false|] |]
    for i in x do
        Array.[i.[0] + 20].[i.[1] + 20] <- true
    Array

let rotate (p:int[]) theta = 
    let x = p.[0]
    let y = p.[1]
    match theta with
    | 90 -> [|-y;x|]
    | 180 -> [|-x;-y|]
    | _ -> [|y;-x|]

let rotateArray p theta = 
    Array.map (fun x -> rotate x theta) p
    |> ToPos


let diffA = 
    [|for i in 0..N-1 ->
        [| for j in 0..N-1 -> 
            [|A.[i].[0] - A.[j].[0]; A.[i].[1] - A.[j].[1]|]
        |]
    |]
   
let juusinn = 
    let x = B |> Array.sumBy (fun x -> x.[0]) B
    let y = B |>  Array.sumBy (fun x -> x.[1]) 
    [|x / N ;y / N|]

let diffB = 
    [|for i in 0..N-1 ->
        [|B.[i].[0] - juusinn.[0];B.[i].[1] - juusinn.[1];|]
    |]
    |> ToPos

let mutable b = false
    for j in diffB do
        if (ToPos i) = j || (rotateArray i 90) = j || (rotateArray i 180) = j || (rotateArray i 270) = j then b <- true


if b = true then "Yes" else "No"
|> printfn "%s"