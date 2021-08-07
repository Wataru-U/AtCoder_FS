let N = stdin.ReadLine() |> int
let mutable H = stdin.ReadLine().Split() |> Array.map int
let mutable ans = 0
let height = Array.max H

let count (arr:int[]) = 
    let mutable c = 0
    let mutable b = true
    for i in arr do
        if i <= 0 then
            b <- true
        if b && i > 0 then
            c <- c + 1
            b <- false
    c


for i in 1..height do
    ans <- ans + (count H)
    H <-
        H 
        |> Array.map (fun x -> x - 1)

printfn "%d" ans