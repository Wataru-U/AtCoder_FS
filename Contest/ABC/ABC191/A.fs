let [|V;T;S;D|]  =stdin.ReadLine().Split() |> Array.map int
if D < V * T || D > V * S 
    then "Yes"
    else "No"
|> printfn "%s"