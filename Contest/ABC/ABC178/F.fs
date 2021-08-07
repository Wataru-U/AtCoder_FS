let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split(' ') |> Array.map int
let mutable  B = stdin.ReadLine().Split(' ') |> Array.map int

let mutable Ab = [|for i in 1 .. N -> 0|]
let mutable Bb = [|for i in 1 .. N -> 0|]
let mutable c = [|for i in 1 .. N -> 0|]
let mutable (k:int[]) = [||]

for i in 0 .. N - 1 do
    Ab.[A.[i]] <- Ab.[A.[i]] + 1
    Bb.[B.[i]] <- Bb.[B.[i]] + 1

let mutable count = 0
for i:int in 0 .. N - 1 do
    let Min = if Ab.[i] < Bb.[i] then Ab.[i] else Bb.[i]
    count <- count + Min
    c.[i] <- Min
    if Min <> 0 then k <- i :: k

let swap a b = 
    let v = B.[a]
    B.[a] <- B.[b]
    B.[b] <- v
    c.[b] <- c.[b] - 1
    c.[a] <- c.[a] - 1


if count < N / 2 
    then printfn "%s" "No"
    else 
        printfn "%s" "Yes"
        for i in 0 .. N-1 do
         printf "%d " B.[i]
