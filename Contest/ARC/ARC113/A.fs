open System.Collections.Generic
open System.Collections
open System
let K = stdin.ReadLine() |> int 
let r = (K |> float |> sqrt |> int) +  1
let mutable ans = 0
for a in 1 .. K do
    let B = K / A
    for b in 1..B do
        let c = B / b
            if a = b = c then ans <- ans + 1