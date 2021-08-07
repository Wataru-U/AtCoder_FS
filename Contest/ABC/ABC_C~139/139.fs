let N = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int

let rec calc index ans v = 
    if index = N then ans
    else
        if A.[index] > A.[index-1] then calc (index+1) ans 0
        else calc (index + 1) (max ans (v+1)) (v+1)

calc 1 0 0
|> printfn "%d"