let calc x = 
    let xx v = if v = 0 then 4 else if v % 3 = 0 then v / 3 else v / 3 + 1
    let x_ =  (x - 1 + 10) % 10 
    let x10_ = x_ + 10
    let x10 = x + 10
    (xx x,xx x_,xx x10,xx x10_)


let T = stdin.ReadLine() |> int
let query = 
    [|
        for i in 1..T -> 
            stdin.ReadLine().ToCharArray()
            |> Array.map (fun x -> x |> string |> int)
    |]

let c = 
    query
    |> Array.map (fun x -> Array.map calc x )

for i in c do
    Array.max i
    |> fun x -> 
        if (x = 2 && i.[0] = 1) || (x = 3 && i.[0] <= 2) 
        then 4
        else x
    |> printfn "%d"
