let rec gcd a b = 
    if a % b = 0L then b else gcd b (a % b)

let lcm a b = 
    a / gcd a b * b

let N = stdin.ReadLine() |> int

let rec calc i v = 
    if i = N then v
    else
        stdin.ReadLine()
        |> int64
        |> lcm v
        |> fun x -> calc (i + 1) x

calc 0 1L
|> printfn "%d"