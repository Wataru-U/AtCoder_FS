let N = stdin.ReadLine() |> int
let T = stdin.ReadLine().Split() |> Array.map int |> Array.sort |> Array.rev
let mutable A = 0L
let mutable B = 0L

let Sum = T |> Array.sum

let rec add index condition v =
    if index = N then max v (Sum - v)
    elif (v + T.[index]) <= condition 
        then
            add (index + 1) condition (v + T.[index])
        else
            add (index + 1) condition (v)
let rec calc up bottom =
    if up - bottom <= 1 
    then
        if add 0 bottom 0 <= bottom then bottom else up
    else
        let condition = (up + bottom) / 2
        let V = add 0 condition 0
        if V <= condition 
        then
            calc condition (max bottom V)
        else
            calc (min up V) condition

calc Sum 0
|> stdout.WriteLine
