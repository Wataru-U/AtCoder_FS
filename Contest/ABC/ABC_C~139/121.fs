let [|N;M|] = stdin.ReadLine().Split() |> Array.map int

let shop = 
    [|for i in 1..N ->
        stdin.ReadLine().Split()
        |> fun x -> ((x.[0]|>int64),(x.[1]|>int) )
    |]
    |> Array.sortBy fst

let mutable ans = 0L
let mutable num = 0

let rec calc index = 
    let n = 
        snd shop.[index]
        |> min (M - num)
    num <- num + n
    ans <- fst shop.[index] * (n |> int64) + ans
    if num = M then ()
    else calc (index + 1)

calc 0

printfn "%d" ans