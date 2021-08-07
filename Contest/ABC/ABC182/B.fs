stdin.ReadLine()
let v = stdin.ReadLine().Split() |> Array.map int |> (fun x -> [| for i in 2 .. 1000 do Array.filter (fun x -> x % i = 0) x |> (fun x -> x.Length) |])
let mutable ans = 0
let mutable max = 0
for i in 0 .. 998 do
    if(max < v.[i]) 
    then 
        ans <- i+2
        max <- v.[i]
ans |> printfn "%d"