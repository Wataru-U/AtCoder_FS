let [|N;P|] = stdin.ReadLine().Split() |> Array.map int64
let MOD = 1000000007L

let rec culc sq v n Mod =
    if n = 0 then v
    else
        let next = n >>> 1
        let nv = if n &&& 1 = 1 then v * sq % Mod else v
        culc (sq * sq % Mod) nv next Mod

let PowNMod e n Mod = 
    culc e 1L n Mod

if N = 1L then printfn "%d" (P-1L)
else
    let n = N - 1L |> int
    PowNMod (P-2L) n MOD
    |> fun x -> x * (P-1L) % MOD
    |> printfn "%d"


