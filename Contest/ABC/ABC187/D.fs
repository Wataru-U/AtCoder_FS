let n = stdin.ReadLine() |> int
let people = [|for i in 1..n -> stdin.ReadLine().Split() |> Array.map int64|]
let aoki = [|for i in 1..n -> people.[i-1].[0]|]
let aokiHyo = Array.sum aoki
let takasiPoint = [|for i in 1 .. n -> aoki.[i-1] * 2L + people.[i-1].[1] |] |> Array.sort |> Array.rev

let rec culc point index =
    if point > aokiHyo then index 
    else
        culc (point + takasiPoint.[index]) (index + 1)
culc 0L 0
|> printfn "%d"