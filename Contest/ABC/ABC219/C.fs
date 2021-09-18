let dic = stdin.ReadLine()
let cn = [|for i in 0..25 -> 0|]
for i in 0..25 do
    cn.[int(dic.[i]) - int('a')] <- i
let change_c c = 
    let n = int(c) - int('a') 
    cn.[n]
let change str =
    [| for i in str -> change_c i|]

let n = stdin.ReadLine() |> int

[|
    for i in 1..n -> 
        let str = stdin.ReadLine()
        (str, change str)
|]
|> Array.sortBy snd
|> fun x ->
    for i in 0..n-1 do
        printfn "%s" (fst x.[i])
