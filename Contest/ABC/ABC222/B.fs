let [|N;P|] = stdin.ReadLine().Split() |> Array.map int
let students = stdin.ReadLine().Split() |> Array.map int
let rec calc i ans =
    if i = N then ans
    elif students.[i] < P then calc (i+1) (ans + 1)
    else calc (i+1) ans

calc 0 0
|> printfn "%d"