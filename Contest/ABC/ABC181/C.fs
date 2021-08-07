let N = stdin.ReadLine() |> int
let v = [|for i in 1 .. N -> stdin.ReadLine().Split(' ') |> Array.map decimal|]

let mutable bo = false
for i in 0..N-3 do
    for j in i+1 .. N-2 do
        for k in j + 1 .. N-1 do
            if v.[k].[0] <> v.[j].[0] && v.[k].[1] <> v.[j].[1]
                then
                    let x = (v.[i].[0] - v.[j].[0]) / (v.[k].[0] - v.[j].[0])
                    let y = (v.[i].[1] - v.[j].[1]) / (v.[k].[1] - v.[j].[1])
                    if x = y then bo <- true
                else
                    if v.[i].[0] = v.[k].[0] && v.[i].[0] = v.[j].[0] || v.[i].[1] = v.[k].[1] && v.[i].[1] = v.[j].[1] then bo <- true //xi = xj = xk 

if bo then printfn "Yes" else  printfn "No"           