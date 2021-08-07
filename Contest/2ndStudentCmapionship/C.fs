let [|A;B|] = stdin.ReadLine().Split() |> Array.map int

let rec sosuu lis (nokori:int[]) = 
    if nokori.Length = 0 then lis
    else
        sosuu (nokori.[0] :: lis) (Array.filter (fun x -> x % nokori.[0] <> 0) nokori)

let lis = sosuu [] [|2..(B |> float |> sqrt |> int |> (fun x -> x + 1))|]

let rec Gcd a b =
    if a % b = 0 then b else Gcd b (a % b)

let mutable ans = 1
for i in A..B do 
    for j in lis do
        if i % j = 0
            then
                if B / j * j >= A then
                    if B / j * j <> i then 
                        ans <- max ans (Gcd  (B / j * j) i)
                let pair = i / j
                let Max = B / pair * pair
                if B >= pair && Max >= A && Max <> i then ans <- max ans (Gcd  (Max) i)
    if B / i <> 1 then ans <- max ans (i)
printfn "%d" ans