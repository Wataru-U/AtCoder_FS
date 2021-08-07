let kaibunn (a:string) = 
    a.ToCharArray()
    |> Array.rev
    |> fun x -> new string(x) 
    |> fun x -> x = a

let rec ans x n = 
    if n = 10 then "No"
    else
        let v = "0" + x
        if kaibunn v then "Yes"
        else ans v (n+1)


let str = stdin.ReadLine()
if kaibunn str then "Yes"
else
    ans str 0
|> printfn "%s"

