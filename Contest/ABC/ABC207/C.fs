let N = stdin.ReadLine() |> int
let query = 
    [|
        for i in 1..N -> 
            stdin.ReadLine().Split() 
            |> Array.map int
    |]

let calc (a:int[]) (b:int[]) = 
    let left = max a.[1] b.[1]
    let right = min a.[2] b.[2]
    if left > right then false
    elif left = right 
        then
            if (a.[1] = left && (a.[0] = 3 || a.[0] = 4)) || (b.[1] = left && (b.[0] = 3 || b.[0] = 4)) then false
            elif (a.[2] = right && (a.[0] = 2 || a.[0] = 4)) || (b.[2] = right && (b.[0] = 2 || b.[0] = 4)) then false
            else true
    else true

let mutable ans = 0
for i in 0..N-2 do
    for j in i+1 .. N-1 do
        if calc query.[i] query.[j] then ans <- ans + 1

printfn "%d" ans