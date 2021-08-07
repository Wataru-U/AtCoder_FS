let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let black = 
    [|for i in 1..M -> stdin.ReadLine().Split() |> Array.map int|]
let y = 
    black 
    |> Array.sortBy (fun x -> abs(N - x.[1]))
let xSort = 
    Array.sort black

let mutable top = N
let mutable bottom = N

for i in y do
    if i.[1] = (top + 1) then top <- (top + 1)
    if i.[1] = (bottom - 1) then bottom <- bottom - 1

let mutable seen = [|for i in bottom.. top -> false|]
seen.[N - bottom] <- true
let mutable last = 0
let mutable lis = []
let mutable o = []
let l = seen.Length
for i in xSort do
    if last <> i.[0]
        then
        for j in lis do
            seen.[j] <- false
        for j in o do 
            seen.[j] <- true
    lis <- []
    o <- []
    last <- i.[0]
    let index = i.[1] - bottom
    if index >= 0 && index < l then
        let mutable b = true
        if index > 0 then
            if seen.[index - 1] 
            then 
                o <- index :: o
                b <- false
        if index < (l-1) && b then
            if seen.[index + 1] then
                seen.[index] <- true
                b <- false
        if b && seen.[index] then lis <- index :: lis
for j in lis do
    seen.[j] <- false
seen
|> Array.filter (fun x -> x)
|> fun x -> x.Length
|> printfn "%d"