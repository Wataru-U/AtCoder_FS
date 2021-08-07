let rec gcd a b = 
    if a % b = 0L then b
    else gcd b (a % b)

let [|A;B;C;D|] = stdin.ReadLine().Split() |> Array.map int64

let lcm = C * D / (gcd C D)


let haba = B - A + 1L
let cn = B / C - A / C + if A % C = 0L then 1L else 0L
let dn = B / D - A / D + if A % D = 0L then 1L else 0L
let ln = B / lcm - A / lcm + if A % lcm = 0L then 1L else 0L

haba - cn - dn + ln
|> printfn "%d"
