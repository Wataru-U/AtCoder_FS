let n = stdin.ReadLine()
let col = stdin.ReadLine().ToCharArray() 

let Do a b = 
    if a = b then a 
    elif a <> 'R' && b <> 'R' then 'R'
    elif a <> 'B' && b <> 'B' then 'B'
    else 'W'

let compression (a:char) (b:char) (c:char) = 
    if a = b && a = c then a
    elif a = c then Do a b
    elif a = b then c
    elif b = c then a
    else b

let rec culc (lis:char[]) =
    if lis.Length = 1 then lis.[0]
    elif lis.Length = 2 then (Do lis.[0] lis.[1])
    else
        culc [|for i in 2..(lis.Length-1) -> compression lis.[i-2] lis.[i-1] lis.[i]|]

culc col
|> printf "%c"
