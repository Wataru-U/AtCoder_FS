let n = stdin.ReadLine() |> int
let mutable per200 = [|for i in 0..199 -> 0L|]
let culc v =
    per200.[v] <- per200.[v] + 1L
stdin.ReadLine().Split() 
|> Array.map int64
|> Array.map (fun x -> x % 200L)
|> Array.map int
|> Array.map culc

let mutable ans = 0L
for i in per200 do
    i
    |> int64
    |> fun x -> ans <- ans + x * (x-1L) / 2L
  
printfn "%d" ans