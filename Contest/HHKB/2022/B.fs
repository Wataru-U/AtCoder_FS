let rec ans i n (arr:int[]) = 
    if i = n - 1 then arr.[i]
    elif arr.[i] >= arr.[i+1] then arr.[i]
    else ans (i+1) n arr

ans 0 
|> fun x -> x (stdin.ReadLine() |> int) 
|> fun x -> x (stdin.ReadLine().Split() |> Array.map int)
|> printfn "%d"