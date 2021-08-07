let v = stdin.ReadLine() |> (fun x -> [|for i in 0..x.Length-1 -> x.[i] |> int|])

let m = v |> Array.map (fun x -> x % 3)
let sum = Array.sum m
let mutable num3 = [|0;0;0|]
for i in m do
    num3.[i] <- num3.[i]+1
if sum % 3 = 0 
    then printfn "0"
    else if sum % 3 = 1
            then if num3.[1] >= 1 && v.Length <> 1 then printfn "1" else if num3.[2] >= 2 && v.Length <> 2 then printfn "2" else printfn "-1"
            else if num3.[2] >= 1 && v.Length <> 1 then printfn "1" else if num3.[1] >= 2 && v.Length <> 2 then printfn "2" else printfn "-1"