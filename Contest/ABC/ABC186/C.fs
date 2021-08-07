let N = stdin.ReadLine() |> int
let mutable count = 0
let con7_10 (v:string) = 
    let mutable b = false
    for i in v do
        if i = '7' then b <- true
    b

let rec con7_8 v s = 
    if v < 8 
        then s + (v |> string)
        else let v_ = v % 8
             con7_8 (v/8) (s+(v_|>string))
        
let contain7 v =
    con7_10 (v |> string) || con7_10 (con7_8 v "")
    
for i in 1 .. N do
    if contain7 i then () else count <- count + 1

printfn "%d" count
