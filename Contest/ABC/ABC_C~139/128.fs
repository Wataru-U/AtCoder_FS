let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let switch = [|for i in 1..M -> stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)|]
let p = stdin.ReadLine().Split() |> Array.map int

let pat = (1 <<< N) - 1
let mutable ans = 0

for i in 0..pat do
    let mutable s = 0
    for j in 0..M-1 do
        let mutable n = 0
        for k in switch.[j].[1..] do
            let bit = (i >>> k) &&& 1
            if bit = 1 then
                n <- n + 1
        if n % 2 = p.[j] then 
            s <- s+1
    if s = M then 
        ans <- ans + 1

ans 
|> printfn "%d"