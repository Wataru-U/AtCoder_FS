let S = stdin.ReadLine()
let T = stdin.ReadLine()
if S = "Y" then
    if T = "a" 
        then printfn "A"
        else if T = "b"
            then printfn "B"
            else printfn "C"
else printfn "%s" T