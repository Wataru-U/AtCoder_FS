let N = stdin.ReadLine() |> int
let weather = stdin.ReadLine()

match weather.[N-1] with
| 'o' -> "Yes"
| _ -> "No"
|> printfn "%s"