let n = stdin.ReadLine() |> int
let pat = pown 2 n |> (fun x -> x - 1)

let rec culc v i one zero str =
    if i = n then 
        if zero = one then printfn "%s" str 
        else ()
    else
        let bit = (v >>> i) &&& 1
        let no = if bit = 1 then one + 1 else one
        let nz = if bit = 0 then zero + 1 else zero
        let nstr = if bit = 1 then ")" + str else "(" + str
        if zero > one then ()
        else
            culc v (i+1) no nz nstr

for i in 0 .. pat do
    culc i 0 0 0 ""
    
        