open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text


// --AutoFlufh-- //
let sw =
    new StreamWriter(Console.OpenStandardOutput())
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)

// --Input-- //
let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let mutable S = 
    [|for i in 1..M -> (stdin.ReadLine(),0)|] 
    |> Array.sortBy (fst >> fun x -> x.Length)

let rand = Random()
// --Lib--- //

let rec isSubseq (a:string) (b:string) aIndex bIndex = 
    if bIndex = b.Length then true
    elif aIndex = a.Length then false
    elif a.[aIndex] = b.[bIndex] then isSubseq a b (aIndex+1) (bIndex+1)
    else isSubseq a b (aIndex - bIndex + 1) 0 

let intA = 'A' |> int



let mutable Ans = []
let mutable LL = [|for i in 2..12 -> []|]

for i in 0..(M-2) do
    let mutable b = false
    for j in (i+1) .. (M-1) do
        if isSubseq (fst S.[j]) (fst S.[i]) 0 0 && not b
        then
            b <- true
            S.[j] <- (fst S.[j],snd S.[j] + snd S.[i] + 1)
    if not b then 
        Ans <- S.[i] :: Ans

Ans <- S.[M-1] :: Ans
Ans <- 
    Ans
    |> List.sortBy snd
    |> List.rev

for i in S do
    let str = fst i
    let l = str.Length - 2
    LL.[l] <- str :: LL.[l]

 
let mutable result = [|for i in 1..N -> [|for i in 1..N -> '.'|]|]
let mutable lineCount = [|for i in 1..N -> 0|]


let mutable index = 0
let mutable str = fst Ans.[0]

let rec setResult i j idx =
    if idx = str.Length then ()
    else
        result.[i].[j] <- str.[idx]
        setResult i (j+1) (idx + 1)

let rec set i j = 
    if str.Length + j >= N then ()
    else 
        setResult i j 0
        let nj = j + str.Length
        lineCount.[i] <- nj - 1
        index <- index + 1
        str <- fst Ans.[index] 
        set i nj

let rec sr i j idx = 
    if idx = str.Length then ()
    else
        result.[i].[j] <- str.[idx]
        sr (i+1) j (idx + 1)

let rec rowset i j = 
    if str.Length + i >= N then ()
    elif result.[str.Length+i].[j] <> '.' then ()
    else 
        sr i j 0
        let ni = i + str.Length
        index <- index + 1
        str <- fst Ans.[index] 
        rowset ni j

for i in 0..(N-1) do
    set i 0

result <- 
    result
    |> Array.sortBy (fun x -> (Array.filter (fun y -> y = '.') x) |> (fun y -> y.Length))
    |> Array.rev

for i in 0..19 do
    rowset 0 ((N-1) - i)

for i in 0..19 do
    let mutable aki = 0
    let mutable maxAki = 0
    let mutable srt = 0
    for j in 0..18 do
        if result.[i].[j] = '.' then
            let mutable b = true
            for k in j .. 19 do
                if result.[i].[k] = '.' && b then aki <- aki + 1
                else b <- false
            if maxAki < aki then 
                maxAki <- aki
                srt <- j
    if maxAki > 2 then
        let l = min maxAki 6
        if LL.[l - 2].Length > 0 then
            let idx = rand.Next(LL.[l - 2].Length)
            printfn "%d %d %A %c" idx l LL.[l-2].[idx] result.[i].[srt]
            stdout.Flush()
            for j in 0..(l - 1) do
                result.[i].[j + srt] <- LL.[l - 2].[idx].[j]
    


for i in 0..(N-1) do
    for j in 0 .. (N-1) do
        if result.[i].[j] = '.' then
            let c = (rand.Next 8) + intA |> char
            result.[i].[j] <- c


for i in 0..(N-1) do
    new string(result.[i])
    |> stdout.WriteLine
    

stdout.Flush()