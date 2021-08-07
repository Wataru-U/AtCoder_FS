let N = stdin.ReadLine() |> int
let mutable A = [|for i in 0..100001 -> 0|]

let calc x = 
    A.[x] <- 1 + A.[x]
    A.[x + 1] <- 1 + A.[x + 1]
    if x > 0 then
        A.[x-1] <- 1 + A.[x-1]
    else ()
            
stdin.ReadLine().Split()
|> Array.map int
|> Array.map calc

A
|> Array.max
|> printfn "%d"