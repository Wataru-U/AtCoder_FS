let [|H;W|] = stdin.ReadLine().Split() |> Array.map int
let A = [|for i in 1..H -> stdin.ReadLine().Split() |> Array.map int|]
let Sum x = Array.sum x
let line = Array.map Sum A
let mutable row = [|for i in 1..W -> 0|]
for i in 0..(H-1) do
    for j in 0..(W-1) do
        row.[j] <- row.[j] +  A.[i].[j]

let sb = System.Text.StringBuilder()
let rec str h w = 
    if w = W then ()
    else 
        " " + ((line.[h] + row.[w] - A.[h].[w]) |> string)
        |> sb.Append
        str h (w + 1)


for i in 0..(H-1) do
    sb.Append ((line.[i] + row.[0] - A.[i].[0]) |> string)
    str i 1
    |> sb.AppendLine
sb.ToString() |> stdout.Write
