let rec gcd a b = 
    match a % b with
    | 0 -> b
    | x -> gcd b x

let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int

let rec calc index v =
    if index = N then v
    else    
        calc (index + 1) (gcd v A.[index])

calc 1 A.[0]
|> printfn "%d"
