let N = stdin.ReadLine() |> int
let S = stdin.ReadLine().ToCharArray() |> Array.map (fun x -> x |> string |> int)

let rec ans index = 
    if S.[index] = 1 
    then 
        if index % 2 = 0 then "Takahashi" else "Aoki"
    else
        ans (index + 1)

ans 0
|> printfn "%s"