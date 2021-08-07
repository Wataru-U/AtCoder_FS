let [|N;A;B;C|] = stdin.ReadLine().Split() |> Array.map int
let l = [|for i in 1..N -> stdin.ReadLine() |> int|]

let pat = (1 <<< (N * 2 + 1)) - 1

let mutable ans = 10000000

for bit in 0..pat do
    
    let mutable len = [|0;0;0;0|]
    let mutable n = [|0;0;0;0|]
    for i in 0..N-1 do
        let idx = (bit >>> (i * 2)) &&& 3
        len.[idx] <- len.[idx] + l.[i]
        n.[idx] <- n.[idx] + 1

    let a = 
        [|A-len.[0];B-len.[1];C-len.[2]|]
        |> Array.map abs
        |> Array.sum
    let b = 
        n.[..2]
        |> Array.map (fun x -> max 0 (x-1))
        |> Array.sum
    let zero = 
        n.[..2]
        |> Array.filter (fun x -> x = 0)
    if zero.Length = 0 then
        ans <- min ans (a + b * 10)

printfn "%d" ans