let [|N;W|] = stdin.ReadLine().Split() |> Array.map int64
let mutable ans = [|for i in 0..200001 -> 0L|]
for i in 1..(N|>int) do
 let [|s;t;p|] = stdin.ReadLine().Split() |> Array.map int64
 for j in s..(t-1L) do
    ans.[(j|>int)] <- ans.[(j|> int)] + p
Array.max ans |> (fun x -> if x > W then "No" else "Yes") |> printfn "%s"