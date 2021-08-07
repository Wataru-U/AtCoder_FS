let s = stdin.ReadLine() 

let t = 
    [|"dream";"dreamer";"erase";"eraser"|]
let l = [|4;6;4;5|]

let mutable ans = [|for i in 0..s.Length -> false|]
ans.[0] <- true

for i in 0..s.Length-1 do
    for j in 0..3 do   
        if ans.[i] && i+l.[j] < s.Length
        then
            if s.[i..i+l.[j]] = t.[j]
            then 
                ans.[i + l.[j] + 1] <- true

ans.[s.Length]
|> function
    | true -> "YES"
    | false -> "NO"
|> printfn "%s"