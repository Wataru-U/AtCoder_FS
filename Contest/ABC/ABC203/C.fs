let [|N;K|] = stdin.ReadLine().Split() |> Array.map int64

let friend = 
    [|for i in 1..(N|>int) -> stdin.ReadLine().Split() |> Array.map int64|]
    |> Array.sort

let rec culc zankin n mura=
    if n = (N|>int) then (zankin + mura)
    elif n = 0 
    then
        if zankin < friend.[n].[0]
            then
                zankin
        else
            culc (zankin - friend.[0].[0] + friend.[0].[1]) 1 friend.[0].[0]
    elif zankin < (friend.[n].[0] - friend.[n-1].[0])
        then (zankin + mura)
        else
            culc (zankin - (friend.[n].[0] - mura) + friend.[n].[1]) (n+1) (friend.[n].[0])

culc K 0 0L
|> printfn "%d"


        

