let N = stdin.ReadLine() |> int

let f = [|for i in 1..N -> stdin.ReadLine().Split() |> Array.map float|]

let t = (f |> Array.sumBy (fun x -> x.[0] / x.[1]) ) / 2.

let rec ans index time v = 
    let li = f.[index].[0]
    let si = f.[index].[1]
    let ti = li / si
    if time > ti 
    then ans (index + 1) (time - ti) (v + li)
    else
        v + (si * time)

ans 0 t 0.
|> printfn "%.15f" 