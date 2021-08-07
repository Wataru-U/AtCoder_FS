let N = stdin.ReadLine()
let l = N.Length
let per2 = l / 2
if l % 2 = 1 then 
    pown 10 per2 
    |> fun x -> x - 1
    |> printfn "%d"
else 
    let simo = N.[per2..] |> int
    let kami = N.[..per2-1] |> int
    if simo >= kami then kami else kami-1
    |> printfn "%d"

