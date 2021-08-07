let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int

let rec calc i r ans =
    if i = N then ans 
    elif A.[i-1] >= A.[i] then calc (i+1) 1L (ans + 1L)
    else
        calc (i+1) (r + 1L) (ans + r + 1L)

calc 1 1L 1L
|> printfn "%d"