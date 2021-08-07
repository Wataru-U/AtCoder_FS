let N = stdin.ReadLine() |> int
let mutable count = [|0;0;0|]
let calc x = 
    if x % 4 = 0 then count.[0] <- count.[0] + 1
    elif x % 2 = 0 then count.[1] <- count.[1] + 1
    else count.[2] <- count.[2] + 1

stdin.ReadLine().Split()
|> Array.map int
|> Array.map calc

if N % 2 = 1 && count.[1] = 0 && count.[0] + 1 >= count.[2] then "Yes"
elif count.[0] >= count.[2] then "Yes"  
else "No"
|> printfn "%s"