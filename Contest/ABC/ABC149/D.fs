let [|N; K|] = stdin.ReadLine().Split(' ') |> Array.map int
let n = N - 1
let [|R;S;P|] = stdin.ReadLine().Split(' ') |> Array.map int
let T = stdin.ReadLine().ToCharArray()
let mutable (b:bool[]) = [|for i in 0..N-1 -> false|]

let point = function
    | 'r' -> P
    | 's' -> R
    | 'p' -> S
    | _ -> 0

let rec sum = function
    | n -> point T.[n]
    | x -> point T.[x] + sum (x+1)

let mutable s = sum 0

for i in K .. n do
    if T.[i - K] = T.[i] && b.[i - K] = false
        then 
            b.[i] <- true
            s <- s - point T.[i]

s |> printfn "%d"

