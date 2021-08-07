open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.IO

let rec toStr (x:int[]) n v= 
    if n = x.Length-1 then (v + (x.[n]|>string))
    else toStr x (n + 1) (v + (x.[n]|>string) + " ")

let exStdIn = new System.IO.StreamReader( "test.txt" )
exStdIn |> System.Console.SetIn
let sw = new StreamWriter("out.txt")
for i in 1..1000 do
    stdin.ReadLine().Split()
    |> fun x -> x.[..3]
    |> Array.map int
    |> fun x -> toStr x 0 ""
    |> sw.WriteLine 
    sw.WriteLine 100
sw.Close()