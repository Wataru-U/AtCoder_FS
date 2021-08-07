let S = stdin.ReadLine().ToCharArray()
let b =
    S
    |> Array.filter (fun x -> x = '0')
    |> fun x -> x.Length

let r = S.Length - b

min b r 
|> fun x -> x * 2
|> printfn "%d"