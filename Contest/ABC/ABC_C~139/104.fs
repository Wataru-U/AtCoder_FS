let [|D;G|] = stdin.ReadLine().Split() |> Array.map int

let p = [|for i in 1 .. D -> stdin.ReadLine().Split() |> Array.map int|]

let pat = (1 <<< D) - 1

let mutable ans = 100000
for i in 0..pat do
    let mutable m = 0
    let mutable point = 0
    let mutable count = 0
    for j in 0..D-1 do
        let bit = (i >>> j) &&& 1
        if bit = 0 then 
            m <- j
        else
            point <- point + (j + 1) * 100 * p.[j].[0] + p.[j].[1]
            count <- count + p.[j].[0]
    let con = (m + 1) * 100 * (p.[m].[0] - 1)
    if point < G && G - point <= con then
        let p = (m + 1) * 100
        count <- count + (G-point) / p + (if (G-point) % p = 0 then 0 else 1)
        point <- G
    if point >= G then
        ans <- min ans count

printfn "%d" ans