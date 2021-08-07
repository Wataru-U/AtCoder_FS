open System
open System.IO

let sw = new StreamWriter("test.txt")
sw |> Console.SetOut

//ここに処理を書く
let r = Random()

let rec print (list: int []) =
    for i in 0 .. list.Length - 2 do
        printf "%d " list.[i]
    printfn "%d" list.[list.Length - 1]

let N = 200
N |> printfn "%d"
for j in 1..N do
    for i in 1..N do
        if i = N 
            then printfn "%d" (r.Next 100)
            else printf "%d " (r.Next 100)
Console.Out.Flush()
