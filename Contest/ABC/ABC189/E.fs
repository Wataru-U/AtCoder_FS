let num = stdin.ReadLine() |> int
let p = Array.init num (fun x -> stdin.ReadLine().Split() |> Array.map int64)
let m = stdin.ReadLine() |> int
let op = Array.init m (fun x -> stdin.ReadLine().Split() |> Array.map int64)
let mutable orign = [|for i in 0 .. m ->[|0L;0L|]|]
let mutable right = [|for i in 0 .. m ->[|1L;0L|]|]
let mutable up = [|for i in 0 .. m ->[|0L;1L|]|]

let inline rotate (func:int64) (p:int64[]) = 
    if func = 1L
        then 
            [|p.[1];-p.[0]|] 
        else
            [|-p.[1];p.[0]|] 
let inline inv (func:int64) (b:int64) (p:int64[]) =
    if func = 3L
        then 
            [| 2L * b - p.[0];p.[1]|] 
        else
            [| p.[0]; 2L * b - p.[1]|]
for i in 1 .. m do
    if op.[i-1].[0] < 3L
        then
            orign.[i] <- rotate op.[i-1].[0] orign.[i-1]
            right.[i] <- rotate op.[i-1].[0] right.[i-1]
            up.[i] <- rotate op.[i-1].[0] up.[i-1]
        else
            orign.[i] <- inv op.[i-1].[0] op.[i-1].[1] orign.[i-1]
            right.[i] <- inv op.[i-1].[0] op.[i-1].[1] right.[i-1]
            up.[i] <- inv op.[i-1].[0] op.[i-1].[1] up.[i-1]

let inline diff (a:int64[]) (b:int64[]) = [|a.[0] - b.[0]; a.[1] - b.[1]|]

let culc (query:int[]) = 
    if query.[0] = 0 
        then 
            p.[query.[1] - 1]
        else
            let s = orign.[query.[0]]
            let r = 
                diff right.[query.[0]] s
                |> Array.map (fun x -> x * p.[query.[1] - 1].[0])
            let u = 
                diff up.[query.[0]] s
                |> Array.map (fun x -> x * p.[query.[1] - 1].[1])
            [|s.[0] + r.[0] + u.[0];s.[1] + r.[1] + u.[1]|]

let q = stdin.ReadLine() |> int
for i in 1..q do
    let qAB = stdin.ReadLine().Split() |> Array.map int 
    culc qAB
    |> fun x -> printfn "%d %d" x.[0] x.[1]

