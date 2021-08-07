let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let AC = [|for i in 1..M -> stdin.ReadLine().Split() |> Array.map int|]

let rec gcd a b = 
    if a % b = 0 then b else gcd b (a % b)

let rec able index sum = 
    if index = M then true
    else
        let ns = (sum + AC.[index].[0]) % N
        let g = gcd N AC.[index].[0]
        if g = 1 then false
        elif ns = 0 then able (index + 1) ns
        elif gcd N ns = 1 then false
        else able (index + 1) ns

if able 0 0 then -1L
else
    let arr = 
        AC
        |> Array.sortBy (fun x -> x.[1])
    if gcd N (N % arr.[0].[0]) = 1 then (N - 1 |> int64) * (arr.[0].[1] |> int64)
    else 
        let a = arr.[0].[0]
        let mutable ans = (arr.[0].[1] |> int64) * ((N-2) |> int64)
        let rec calc index x =
            if gcd N (a + AC.[index].[0]) = 1 then AC.[index].[1] |> int64
            else calc (index + 1) x 
        ans + calc 1 arr.[0].[0]
|> printfn "%d"



