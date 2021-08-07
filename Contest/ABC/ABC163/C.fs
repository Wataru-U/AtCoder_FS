let n = stdin.ReadLine() |> int
let mutable buka = [|for i in 1..n -> 0|]
let boss = stdin.ReadLine().Split() |> Array.map (fun x -> (x|>int)-1)
for i in boss do
    buka.[i] <- buka.[i] + 1
for i in buka do
    printfn "%d" i