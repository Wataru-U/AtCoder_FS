open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text

let N = stdin.ReadLine() |> int
let usr = [| for i in 1..N -> stdin.ReadLine().Split() |> Array.map int|]

let mutable lis = [|for i in 1..2 * N -> (0,0)|]
for i in 0..N-1 do
    let item = usr.[i]
    let index = i * 2
    lis.[index] <- (item.[0],1)
    lis.[index + 1] <- (item.[0] + item.[1],-1)
    
lis <- Array.sortBy fst lis

let ans = [| for i in 1..N -> 0|]
let rec calc i d n = 
    if i = lis.Length then ()
    else
        let ivent = lis.[i]
        if fst ivent = d then calc (i+1) d (n + snd ivent)
        else
            let v = fst ivent - d
            do if n > 0 then ans.[n-1] <- v + ans.[n-1]
            calc (i+1) (fst ivent) (n + snd ivent)

calc 0 1 0

let sb = System.Text.StringBuilder()

for i in 0..N-2 do
    string(ans.[i]) + " " |> sb.Append

string(ans.[N-1]) |> sb.AppendLine

sb.ToString() 
|> stdout.Write


