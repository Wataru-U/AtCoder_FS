// ---- input ---- //

let N = stdin.ReadLine() |> int
let P = stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)
let Q = stdin.ReadLine().Split() |> Array.map int |> Array.map (fun x -> x-1)

// ---- input ---- //

// ---- lib ---- //
let rec fac x = 
    if x = 1 then 1 else x * (fac (x-1))

let rec calc index (seen : bool[]) v (arr:int[]) pat = 
    if index = N-1 then v
    else
        seen.[arr.[index]] <- true
        let np = pat / (N - index)
        let mae = 
            seen.[..arr.[index]]
            |> Array.filter (fun x -> not x)
            |> Array.length
        let nv = v + np * mae
        calc (index+1) seen nv arr np

// ---- lib ---- //

let patern = fac N
let pNum = calc 0 [|for i in 1..N -> false|] 0 P patern
let qNum = calc 0 [|for i in 1..N -> false|] 0 Q patern

pNum - qNum
|> abs
|> printfn "%d"