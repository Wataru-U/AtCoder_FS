let [|N;Q|] = stdin.ReadLine().Split() |> Array.map int
let A = 
    stdin.ReadLine().Split() 
    |> Array.map int64
    |> Array.sort

let mutable ketuban = [|for i in 0..N-1 -> 0L|]
let mutable parent = [|for i in 0..N-1 -> 0|]
ketuban.[0] <- A.[0] - 1L

for i in 0..N-2 do
    ketuban.[i+1] <- A.[i+1] - A.[i] - 1L + ketuban.[i]
    if ketuban.[i+1] = ketuban.[i] then parent.[i + 1] <- parent.[i]
    else parent.[i+1] <- i+1 

let rec calc top bottom v = 
    if top - bottom = 1 then
        if ketuban.[top] < v then top else bottom
    elif top - bottom = 0 then bottom
    else
        let index = ( top + bottom ) / 2
        if ketuban.[index] = v then (parent.[index] - 1)
        elif ketuban.[index] < v then calc top index v
        else calc index bottom v

for i in 1..Q do
    let q = stdin.ReadLine() |> int64
    if q <= ketuban.[0] then q
    elif q > ketuban.[N-1] then (A.[N-1] + q - ketuban.[N-1] )
    else 
        let index = calc (N-1) 0 q
        (A.[index] + q - ketuban.[index])
    |> printfn "%d" 
