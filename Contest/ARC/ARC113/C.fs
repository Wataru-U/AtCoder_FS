open System.Collections.Generic
type hoge = 
    val mutable index : int
    val mutable v : char
    new(h,V) = {index = h;v = V}
let flag = List<hoge>()
let S = stdin.ReadLine().ToCharArray()
let N = S.Length
let mutable ans = 0
for i in 2..N-1 do
    if S.[i-2] = S.[i-1] && S.[i] <> S.[i-1]
        then
            let mutable b = true
            if flag.Count <> 0 
                then
                    for j in flag do
                        if S.[i-1] = j.v
                            then  
                                b<-false
            if b 
                then 
                    S.[i..]
                    |> Array.filter (fun x -> x <> S.[i-1])
                    |> fun x -> 
                        ans <- ans + x.Length
                        flag.Add(hoge(0,S.[i-1]))
ans
|> printfn "%d"