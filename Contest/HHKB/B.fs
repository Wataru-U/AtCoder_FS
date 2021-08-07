let [|H;W|] = stdin.ReadLine().Split(' ') |> Array.map int
let s = [|for i in 1 ..H  -> stdin.ReadLine()|]

let mutable c = 0
for i in 0 .. H - 1 do
    for j in 0 .. W-1 do
        if j <> W - 1 then
            if s.[i].[j] = '.' && s.[i].[j+1] = '.' then c <- c+1
        if i <> H - 1 then
            if s.[i].[j] = '.' && s.[i+1].[j] = '.' then c <- c+1

printfn "%d" c            