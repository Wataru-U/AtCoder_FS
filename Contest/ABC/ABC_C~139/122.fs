let [|N;Q|] = stdin.ReadLine().Split() |> Array.map int
let S = stdin.ReadLine()
let query = 
    [|for i in 1..Q -> 
        stdin.ReadLine().Split() 
        |> Array.map int 
        |> Array.map (fun x -> x-1)
    |]

let mutable AC = [|for i in 1..N -> 0|]


for i in 1..N-1 do
    if S.[i-1] = 'A' && S.[i] = 'C'
    then 
        AC.[i] <- AC.[i-1] + 1
    else
        AC.[i] <- AC.[i-1]

let ans (x:int[]) = 
    AC.[x.[1]] - AC.[x.[0]]


query 
|> Array.map (fun x -> ans x)
|> Array.map (fun x -> printfn "%d" x)