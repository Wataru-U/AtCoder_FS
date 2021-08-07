let N = stdin.ReadLine() |> int
let l = 
    N 
    |> string 
    |> fun x -> x.Length
let pow3 = 
    [|for i in 0..l+1 -> pown 3 i|]

let pat = pown 3 (l + 1) - 1

let mutable ans = 0

for bit in 0..pat do
    let mutable str = "0"
    let mutable count = [|0;0;0|]
    for i in 0..l do
        if bit > pow3.[i+1]
        then
            let v = (bit % pow3.[i+1]) / pow3.[i]
            count.[v] <- 1
            let s = 
                match v with 
                | 0 -> "3"
                | 1 -> "5"
                | _ -> "7"
            str <- str + s
    let c = Array.filter (fun x -> x = 1) count
    if (str |> int) <= N && c.Length = 3 then
        ans <- ans + 1

ans / 2
|> printfn "%d"