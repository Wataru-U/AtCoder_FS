let rec gcd a b = if a % b = 0 then b else gcd b (a % b)

let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int

let mutable L = [|for i in 0 .. N-1 -> A.[i]|]
let mutable R = [|for i in 0 .. N-1 -> A.[i]|]


for i in 1..N-1 do
    L.[i] <- gcd L.[i-1] L.[i]
    let index = N - i - 1
    R.[index] <- gcd R.[index+1] R.[index]

let mutable ans = max L.[N-2] R.[1]
for i in 1..N-2 do
    ans <- max ans (gcd L.[i-1] R.[i+1])

printfn "%d" ans