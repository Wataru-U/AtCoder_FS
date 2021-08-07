let [|N;S;D|] = stdin.ReadLine().Split() |> Array.map int64
let n = N |> int
let mutable ans = false
for i in 0..(n-1) do
    stdin.ReadLine().Split()
    |> Array.map int64
    |> fun x -> if x.[0] < S && x.[1] > D then ans <- true

ans 
|> function
| true -> "Yes"
| _ -> "No"
|> printfn "%s"