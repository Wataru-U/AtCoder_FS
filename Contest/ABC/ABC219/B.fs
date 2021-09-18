let s1 = stdin.ReadLine()
let s2 = stdin.ReadLine()
let s3 = stdin.ReadLine()
let num = stdin.ReadLine()

let mutable ans = ""

for i in 0..(num.Length - 1) do
    let n = num.[i] |> string |> int
    match n with
        | 1 -> s1
        | 2 -> s2
        | 3 -> s3
    |> string
    |> fun x -> ans <- ans + x

printfn "%s" ans