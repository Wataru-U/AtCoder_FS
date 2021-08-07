open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text
let sw =
    new StreamWriter(Console.OpenStandardOutput())
    |> fun x -> x.AutoFlush <- false; x
Console.SetOut(sw)
let N = stdin.ReadLine() |> int
let A = 
    stdin.ReadLine().Split() 
    |> Array.map int
    |> Array.sort
let q = stdin.ReadLine() |> int
let b = [|for i in 1..q -> [|(stdin.ReadLine() |> int);(i-1)|]|]

let B = 
    b
    |> Array.sort

let rec calc (ans:int[]) n index v next = 
    if n = q then ans
    elif B.[n].[0] > next 
        then
            if index = (N-1) then
                ans.[B.[n].[1]] <- abs(next - B.[n].[0])
                calc ans (n+1) index v next
            else
                calc ans n (index+1) next (A.[index + 1])
    else
        ans.[B.[n].[1]] <- min (abs(B.[n].[0] - v)) (abs(B.[n].[0] - next))
        calc ans (n+1) index v next

if N = 1 
then
    b
    |> Array.map (fun x -> abs (A.[0] - x.[0]))
else
    calc [|for i in 1..q -> 0|] 0 1 A.[0] A.[1]
|> fun x ->
    for i in x do
        printfn "%d" i
stdout.Flush()