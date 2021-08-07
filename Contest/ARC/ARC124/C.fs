let rec gcd a b = 
    if a % b = 0L then b else gcd b (a%b)

let yakusuu v =
    let mutable result = []
    v
    |> float
    |> sqrt
    |> int64
    |> fun x -> 
        for i in 1L..x+1L do
            if v % i = 0L 
            then 
                result <- i :: result
                result <- (v/i) :: result
    result

let N = stdin.ReadLine() |> int

let query = 
    [|
        for i in 0..N-1 -> 
            stdin.ReadLine().Split() 
            |> Array.map int64
            |> Array.sort
    |]
    |> Array.sortBy (fun x -> x.[1])

let mutable blue = query.[0].[0]
let mutable red = query.[0].[1]

let b_ = 
    yakusuu blue
let r_ =
    yakusuu red

let rec ans index (x: int64 * int64) = 
    if index = N then fst x * snd x / gcd (fst x) (snd x)
    else
        let i = query.[index]
        let bg = Array.map (fun y -> y % fst x) i
        let ag = Array.map (fun y -> y % snd x) i
        if (bg.[0] = 0L && ag.[1] = 0L) || (bg.[1] = 0L && ag.[0] = 0L)
        then ans (index + 1) x
        else 1L

let rec calc index (s: (int64 * int64) [])= 
    if index = s.Length - 1 then 1L
    else
        let r = ans 0 s.[index]
        if r <> 1L then r
        else calc (index + 1) s
    
[|
    for i in b_ do
        for j in r_ do
            (i,j)
|]
|> Array.sortBy (fun (x,y) -> x * y / (gcd x y))
|> Array.rev
|> fun x -> calc 0 x
|> printfn "%d"