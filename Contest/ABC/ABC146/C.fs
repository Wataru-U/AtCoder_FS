let [|A;B;X|] = stdin.ReadLine().Split() |> Array.map int64
let check v = 
    let a = v |> int |> string |> (fun x -> x.Length) |> int64
    A * v + B * a  <= X

let rec calc t b = 
    if t - b = 1L then 
        if check t then t else b
    else
        let center = (t + b) / 2L
        if check center then calc t center else calc center b

let top = 1e+9 |> int64


if check top then top
else calc top 0L
|> printfn "%d"