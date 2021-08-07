let N = stdin.ReadLine() |> int
let a = stdin.ReadLine().Split() |> Array.map int
let b = stdin.ReadLine().Split() |> Array.map int

let ab = 
    [|
        for i in 0..N-1 ->
        [|
            for j in 0..N-1 -> (a.[i] ^^^ b.[j] , j)
        |]
        |> Array.sortBy fst
    |]

let mutable indexes = [|for i in 1..N -> 0|]
let mutable (ans: int list) = []

let rec calc x v (arr: bool []) =
    if x = 0
    then 
        if indexes.[0] = N then ()
        else 
            indexes.[0] <- indexes.[0] + 1
            calc 1 (fst ab.[0].[indexes.[0]-1]) [|for i in 1..N -> false|]
    elif x = N 
        then 
            ans <- v :: ans
            calc 0 0 arr
    elif indexes.[x] = N then ()
    else 
        if fst ab.[x].[indexes.[x]] = v && not arr.[snd ab.[x].[indexes.[x]]] 
        then 
            indexes.[x] <- indexes.[x] + 1
            let mutable na = arr
            na.[snd ab.[x].[indexes.[x] - 1]] <- true
            calc (x + 1) v na
        else
            if v < fst ab.[x].[indexes.[x]] 
            then calc 0 0 arr
            else 
                indexes.[x] <- indexes.[x] + 1
                calc x v arr

calc 0 0 [||]

ans
|> List.rev
|> fun x -> 
    printfn "%d" x.Length
    for i in x do
        printfn "%d" i