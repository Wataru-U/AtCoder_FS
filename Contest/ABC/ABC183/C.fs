let rec check N (seen:int[]) k sum (array:int64[][]) num  =
    if seen = [|for i in 1 .. N -> 1|]
        then
            if (sum + array.[num].[0]) = k then 1 else 0
        else
            let mutable ans = 0
            for i in 1 .. N-1 do
                if seen.[i] = 0
                    then 
                        printfn "%A %d" seen sum
                        let s = [|for j in 0 .. N-1 -> if j = i then 1 else seen.[j] |]
                        ans <- ans + check N s k (sum+array.[num].[i]) array i
            ans

let [|N;K|] = stdin.ReadLine().Split() |> Array.map int64

let v = [|for i in 1 .. (N|>int) -> stdin.ReadLine().Split() |> Array.map int64|]
let visit = [|for i in 1 ..(N|>int) -> if i = 1 then 1 else 0|]


check (N|>int) visit K 0L v 0 |> printfn "%d"