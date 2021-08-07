let [|H; W; N; M|] = stdin.ReadLine().Split() |> Array.map int
let l = [| for i in 1 .. N -> stdin.ReadLine().Split() |> Array.map (fun x -> x |> int) |> Array.map (fun x -> x - 1)|]
let b = [| for i in 1 .. M -> stdin.ReadLine().Split() |> Array.map (fun x -> x |> int) |> Array.map (fun x -> x - 1)|]

let pos = [|for i in 1..W -> [|for j in 1 .. H -> 0|]|]


let rec lighted (p:int[][]) x y (xd:int) (yd:int) = 
    if (x = -1 || x=W || y = -1 || y=H)
        then ()
        else
            if p.[x].[y] = 0 
                then 
                    p.[x].[y] <- 3 
                    lighted p (x+xd) (y+yd) xd yd                   

for item in l do
    pos.[item.[0]].[item.[1]] <- 1

for item in b do
    pos.[item.[0]].[item.[1]] <- 2

let u = Array.copy pos
let d = Array.copy u
let r = Array.copy d
let lef = Array.copy r


for item in l do
    lighted u (item.[0]+1) item.[1] 1 0
    lighted d (item.[0]-1) item.[1] -1 0
    lighted r (item.[0]) (item.[1]+1) 0 1
    lighted lef (item.[0]) (item.[1]-1) 0 -1

let ans = [|for i in 0 .. H-1 -> [|for j in 0 .. W-1 -> if(u.[i].[j] = 1 || u.[i].[j] = 3 || d.[i].[j] = 1 || d.[i].[j] = 3 || r.[i].[j] = 1 || r.[i].[j] = 3 || lef.[i].[j] = 1 || lef.[i].[j] = 3 ) then 1 else 0 |]|]

[|for i in 0 .. W-1 -> Array.filter (fun elm -> elm = 1) ans.[i] |> (fun x -> x.Length)|]
|> Array.sum 
|> printfn "%d"