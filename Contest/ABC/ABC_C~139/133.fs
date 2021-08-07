let [|L;R|] = stdin.ReadLine().Split() |> Array.map int
if R - L >= 2019 then 0
else
    let arr = [|for i in L .. R -> i  % 2019|]
    let mutable min = 2019
    for i in 0..(arr.Length - 2) do
        for j in (i+1)..(arr.Length - 1) do
            if min > (arr.[i] * arr.[j]) % 2019 then min <-  (arr.[i] * arr.[j]) % 2019
    min  
|> printfn "%d"