let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let A = [|for i in 1..N -> stdin.ReadLine()|]
let B = [|for i in 1..M -> stdin.ReadLine()|]

let rec check i j = 
    if i + M = N then false
    elif j + M = N then check (i+1) 0
    else
        [|
            for k in 0..M-1 ->
                let a = A.[i + k].[j..j+M-1]
                let b = B.[k]
                a = b
        |]
        |> Array.filter (fun x -> x)
        |> Array.length
        |> (=) M
        |> function 
            | true -> true
            | false -> check i (j+1)

if N = M then A = B
else check 0 0
|> function 
    | true -> "Yes"
    | false -> "No"
|> printfn "%s"