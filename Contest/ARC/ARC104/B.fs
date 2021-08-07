let [|N;S|] = stdin.ReadLine().Split(' ')
let n = N |> int
let s = let mutable count = 0
        for i in 0.. n-2 do
                let mutable a = 0
                let mutable t = 0
                let mutable c = 0
                let mutable g = 0
                for j in i..n-1 do
                    if S.[j] = 'A' then a <- a + 1
                    if S.[j] = 'T' then t <- t + 1
                    if S.[j] = 'C' then c <- c + 1
                    if S.[j] = 'G' then g <- g + 1
                    if  a = t && c = g then count <- count + 1
        count
 
printfn "%d" s
System.Console.Out.Flush() 