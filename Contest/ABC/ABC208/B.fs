let fac = 
    [|
        for i in 1..10 ->
            let mutable x = 1
            for j in 2..i do
                x <- x * j
            x
    |]

let mutable P = stdin.ReadLine() |> int
let mutable ans = 0

for i in 0..9 do
    let index = 9 - i
    if P >= fac.[index] then
        ans <- ans + P / fac.[index]
        P <- P - (P / fac.[index]) * fac.[index]

printfn "%d" ans
