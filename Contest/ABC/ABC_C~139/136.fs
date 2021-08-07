

let N = stdin.ReadLine()|> int
let mutable A = 
    stdin.ReadLine().Split()
    |> Array.map int
 
A.[0] <- A.[0] - 1

let rec ans index = 
    if index = N then "Yes"
    else
        A.[index] <- if A.[index] - 1 >= A.[index - 1] then A.[index] - 1 else A.[index]
        if A.[index] >= A.[index-1] then ans (index + 1)
        else "No"

ans 1
|> printfn "%s"
