let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let q = [|for i in 1..M -> stdin.ReadLine().Split() |> Array.map int|]
let mutable (a:list<int64[]>[]) = [|for i in 0..N -> []|]

for i in q do
    a.[i.[0]] <- (i.[1..] |> Array.map int64):: a.[i.[0]]
let mutable fac = 1L
let mutable No = 0L

for i in 1..N do
    fac <- fac * (i|>int64)
    for j in a.[i] do
        let mutable c = j.[0]
        let mutable a_2 = j.[1] + 1L
        let mutable amari = (N|>int64) - c
        let mutable petern  = 1L
        for k in 1..i do
            if a_2 = 0L then
                petern <- petern * amari
                amari <- amari - 1L
            else
                petern <- c * petern
                c <- c - 1L
            a_2 <- a_2 - 1L
        No <- No + petern
fac - No 
|> printfn "%d"
