// 約数全列挙　 x２

let [|N;M|] = stdin.ReadLine().Split() |> Array.map int
let A = stdin.ReadLine().Split() |> Array.map int
let mutable seen = [|for i in 0..M -> false|]
let mutable Ans = [1]
let see v = 
    let m = v |> float |> sqrt |> int |> (fun x -> x + 1)
    do
        for i in 1..(min m M) do
            if v % i = 0 
            then
                seen.[i] <- true
                if v / i <= M then
                    seen.[v/i] <- true   
let rec ans v = 
    let m = v |> float |> sqrt |> int |> (fun x -> x + 1)
    let mutable b = false
    do
        for i in 1..(min m M) do
            if v % i = 0
            then
                if seen.[i] && i <> 1 then b <- true
                elif v / i <= M && v <> i then
                if seen.[v/i] then b <- true 
    if not b then Ans <- v :: Ans
    else ()
            

Array.map see A
for i in 1..M do
    if not seen.[i] then ans i

printfn "%d" Ans.Length
Ans 
|> List.rev
|> List.map (fun x -> printfn "%d" x)