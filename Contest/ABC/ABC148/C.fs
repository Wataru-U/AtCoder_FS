let [|A;B|] = stdin.ReadLine().Split() |> Array.map int64

let rec gcd a b = 
    if a % b = 0L then b else gcd b (a%b)

let g = gcd (min A B) (max A B)

A * B / g
|> printfn "%d"