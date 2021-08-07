let [|H;W|] = stdin.ReadLine().Split() |> Array.map int
let grid = [|for i in 1..H -> stdin.ReadLine()|]

let mutable prev = 0
let mutable ans = 0
let mutable add = true
let mutable diff = 0

for i in 0 .. H-1 do
    let mutable n = 0
    for j in 0 .. W-1 do
        if grid.[i].[j] = '#'
            then n <- n+1
    if prev = 0
        then 
            if n > 1 then ans <- ans + 3
        else 
            if diff <> n - prev
                then
                    if n = 0 && prev > 1 
                        then ans <- ans + 1
                        else
                            if abs (n - prev) = 1 
                                then ans <- ans + 1
                                else 
                                    if abs (n - prev) = 2 then ans <- ans + 2

    diff <- n - prev
    prev <- n
if prev 

printfn "%d" ans