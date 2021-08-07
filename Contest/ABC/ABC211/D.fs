let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let MOD = (1e+9 |> int64) + 7L

let pass = 
    [|
        for i in 1..M -> 
            stdin.ReadLine().Split() 
            |> Array.map int 
            |> Array.map (fun x -> x - 1)
    |]

let mutable (To: int list []) = 
    [|
        for i in 1..N -> []
    |]

for p in pass do
    To.[p.[0]] <- p.[1] :: To.[p.[0]]
    To.[p.[1]] <- p.[0] :: To.[p.[1]]

let mutable dist = [|for i in 1..N -> -1|]
let mutable ans = [|for i in 1..N -> 0L|]
let mutable queue = [|for i in 1..N -> 0|]

let rec bfs index tail = 
    if index = N then ()
    else
        let i = queue.[index]
        let mutable tidx = tail
        for t in To.[i] do
            do
                if dist.[t] = -1 
                then    
                    dist.[t] <- dist.[i] + 1
                    ans.[t] <- ans.[i]
                    queue.[tidx] <- t
                    tidx <- tidx + 1
                elif dist.[t] = dist.[i] + 1 then
                    ans.[t] <- (ans.[t] + ans.[i]) % MOD
        bfs (index + 1) tidx
dist.[0] <- 0
ans.[0] <- 1L
bfs 0 1

printfn "%d" ans.[N-1]