let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let A = 
    stdin.ReadLine().Split() 
    |> Array.map int64 
    |> Array.sort
    |> Array.rev
let B = 
    [|for i in  1..M -> stdin.ReadLine().Split() |> Array.map int|] 
    |> Array.sortBy (fun x -> x.[1])
    |> Array.rev

let mutable D = Array.init N (fun _ -> 0L)
let mutable index = 0

let rec md i = 
    if index = N || i = M then ()
    else
        do
            let mutable j = 0
            while j < B.[i].[0] && index < N do
                D.[index] <- (B.[i].[1] |> int64)
                index <- index + 1
                j <- j + 1
        md (i+1)

md 0

D <- Array.rev (D.[0..index-1])

let mutable ruisekiwa = A
let mutable Dsum = D |> Array.sum

for i in 1..N-1 do
    ruisekiwa.[i] <- ruisekiwa.[i] + ruisekiwa.[i-1]

let mutable ans = 
    if index = N then Dsum
    else ruisekiwa.[N - index - 1] + Dsum

for i in 0..index-1 do
    let Ai = N - index + i
    let Di = i
    Dsum <- Dsum - D.[Di]
    ans <- max ans (ruisekiwa.[Ai] + Dsum)
    
printfn "%d" ans