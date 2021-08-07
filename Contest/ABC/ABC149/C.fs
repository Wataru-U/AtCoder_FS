// input
let N = stdin.ReadLine() |> int

// 準備
let mutable sosuu = []

let rec choose (arr:int[]) = 
    sosuu <- arr.[0] :: sosuu
    if arr.Length = 1 then ()
    else
        arr
        |> Array.filter (fun x -> x % arr.[0] <> 0)
        |> choose

let rec calc index =
    if sosuu.[index] = N then sosuu.[index]
    elif sosuu.[index] < N then sosuu.[index - 1]
    else calc (index + 1)


choose [|2..(N + 200)|]

calc (0)
|> printfn "%d"
