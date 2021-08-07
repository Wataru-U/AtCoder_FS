let [|N;K|] = stdin.ReadLine().Split() |> Array.map int

let MOD = 998244353L

let mutable ans = 1L

let query = [|for i in 1..K -> stdin.ReadLine().Split() |]

let mutable l = [|for i in 1..N -> -1|]
let mutable r = [|for i in 1..N -> -1|]

for i in 1..K do
    let In = query.[i-1]
    if In.[0] = "L" then
        l.[(In.[1] |> int) - 1] <- i-1
    else
        r.[(In.[1] |> int) - 1] <- i-1

let mutable lc = 
    l
    |> Array.filter (fun x -> x <> -1)
    |> Array.length
    |> int64

let mutable rc = 0L

for i in 0..N-1 do
    if l.[i] <> -1 then lc <- lc - 1L
    elif r.[i] > -1 
    then rc <- rc + 1L
    else ans <- (ans * ((K|>int64)-lc-rc)) % MOD

printfn "%d" ans