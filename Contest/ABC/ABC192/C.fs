let [|N;k|] = stdin.ReadLine().Split()
let K = k |> int
let Sort (str:string) = 
    let mutable r = [|for i in 0..9 -> 0|]
    for i in str do
        r.[i.ToString() |> int] <- r.[i.ToString() |> int] + 1
    r
let Min (arr:int[]) = 
    let mutable ans = ""
    for i in 0..9 do
        for j in 1 .. arr.[i] do
            ans <- ans + i.ToString()
    ans
let Max (arr:int[]) = 
    let mutable ans = ""
    for i in 0..9 do
        for j in 1 .. arr.[9-i] do
            ans <- ans + (9-i).ToString()
    ans
let mutable ans = N
for i in 1 .. K do
    let sorted = Sort ans
    let mini = Min sorted
    let maxi = Max sorted
    let n = (maxi |> int64) - (mini|>int64)
    ans <- n.ToString()
ans |> printfn "%s"
