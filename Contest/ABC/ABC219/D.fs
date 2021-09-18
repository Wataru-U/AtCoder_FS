let N = stdin.ReadLine() |> int
let [|X;Y|] = stdin.ReadLine().Split() |> Array.map int
let bento = 
    [|
        for i in 1..N ->
            stdin.ReadLine().Split() |> Array.map int
    |]
let dp = [|for i in 0..X -> [|for j in 0..Y -> 0|]|]

let mutable ans = 1000000
for k in bento do
    let x = k.[0]
    let y = k.[1]
    if x >= X && y >= Y then ans <- 1
    else
    for I in 1..X do
        let i = X - I + 1
        for J in 1..Y do
                let j = Y - J + 1
                if i = x && j = y 
                then 
                    dp.[i].[j] <- 1
                elif dp.[i].[j] <> 0
                then 
                    let v = dp.[i].[j] + 1
                    if i + x >= X && j + y >= Y then ans <- min ans v
                    elif i + x >= X
                    then
                        if dp.[X].[j+y] = 0 || dp.[X].[j+y] > v
                            then
                                dp.[X].[j+y] <- v
                    elif j + y >= Y
                    then
                        if dp.[i+x].[Y] = 0 || dp.[i+x].[Y] > v
                            then
                                dp.[i+x].[Y] <- v
                    else
                        if dp.[i+x].[j+y] = 0 || dp.[i+x].[j+y] > v
                            then
                                dp.[i+x].[j+y] <- v
    if x >= X && y < Y then dp.[X].[y] <- 1
    elif x < X && y >= Y then dp.[x].[Y] <- 1
    
if dp.[X].[Y] <> 0
then min dp.[X].[Y] ans
elif ans <> 1000000
then ans
else -1
|> printfn "%d"