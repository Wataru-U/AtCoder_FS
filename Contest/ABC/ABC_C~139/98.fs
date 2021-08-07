let N = stdin.ReadLine() |> int

let S = stdin.ReadLine().ToCharArray()

let east = 
    S
    |> Array.filter (fun x -> x = 'E')
    |> fun x -> x.Length

let westNum = [|for i in 1..N -> 0|]
let eastNum = [|for i in 1..N -> east|]

Array.set westNum 0 (if S.[0] = 'W' then 1 else 0)
Array.set eastNum 0 (east - if S.[0] = 'E' then 1 else 0)

for i in 1..N-1 do
    let w = if S.[i] = 'W' then 1 else 0
    let e = if S.[i] = 'E' then 1 else 0

    Array.set westNum i (westNum.[i-1] + w)
    Array.set eastNum i (eastNum.[i-1] - e)

let mutable ans = min eastNum.[0] westNum.[N-1]
for i in 0..N-2 do
    ans <- min ans (westNum.[i] + eastNum.[i+1])

printfn "%d" ans