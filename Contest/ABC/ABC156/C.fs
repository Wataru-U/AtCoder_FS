let N = stdin.ReadLine() |> int
let X = stdin.ReadLine().Split() |> Array.map int
let ave = (Array.sum X) / N
let diff = Array.map (fun x -> pown (ave-x) 2) X
let diff2 = Array.map (fun x -> pown (ave-x + 1) 2) X

min (Array.sum diff) (Array.sum diff2)
|> printfn "%d"
