let rec gcd a b = if a % b = 0 then b else gcd b (a % b)
let mutable ans = 0
let K = stdin.ReadLine() |> int
for i in 1..K do
    for j in 1..K do
        for k in 1..K do
            ans <- ans + (gcd i (gcd j k))
printfn "%d" ans