let n = stdin.ReadLine() |> int 
let rec ans count = 
    if count = n then ()
    else
        stdin.ReadLine()
        |> int64
        |> fun x ->
            if x % 4L = 0L then "Even"
            elif x % 2L = 0L then "Same"
            else "Odd"
        |> printfn "%s"
        ans (count+1)
ans 0