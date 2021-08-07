let  [|N; M|] = stdin.ReadLine().Split(' ') |> Array.map int

let mutable (b:bool[]) = [|for i in 0 .. N -> false|]
let mutable a = [|0 .. N|]
let mutable m = false
 
for i in 1 .. M do
    let [|s; c|] = stdin.ReadLine().Split(' ') |> Array.map int
    if b.[s] = false 
        then
            b.[s] <- true
            a.[s] <- c
        else if a.[s] <> c 
            then
                m <- true
             
if m = true || a.[1] = 0 && N <> 1 then
        printf "%d" -1
    else
        let mutable ans = ""
        for i in 1 .. N do 
            let j =
                if b.[i] = true 
                    then a.[i] 
                    else if i <> 1 || N = 1 then 
                        0
                        else 1         
            ans <- ans + j.ToString()
        printfn "%s" ans



