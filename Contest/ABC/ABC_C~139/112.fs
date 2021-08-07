let N = stdin.ReadLine() |> int

let pos = 
    [|for i in 1..N -> stdin.ReadLine().Split() |> Array.map int|]

let std = 
    pos
    |> Array.filter (fun x -> x.[2] <> 0)
    |> Array.head

let mutable ans_h = 0
let mutable ans_x = 0
let mutable ans_y = 0

let calc x y (arr:int[]) = 
    let a = x - arr.[0] |> abs
    let b = y - arr.[1] |> abs
    arr.[2] + a + b

let rec check x y index h = 
    if index = N then h
    else
        let p = pos.[index]
        let a = x - p.[0] |> abs
        let b = y - p.[1] |> abs
        let locH = max (h - a - b) 0
        if locH = p.[2] then check x y (index + 1) h
        else 0 

for x in 0..100 do
    for y in 0..100 do
        let h = check x y 0 (calc x y std)
        if h <> 0 then
            ans_h <- h
            ans_x <- x
            ans_y <- y

printfn "%d %d %d" ans_x ans_y ans_h