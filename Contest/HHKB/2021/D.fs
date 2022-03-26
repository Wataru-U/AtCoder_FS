let T = stdin.ReadLine() |> int
let MOD = 1000000007L
let PowMOD (a:int64) n =
    let mutable ans = 1L
    for i in 1 .. n do
        ans <- ans * a % MOD
    ans
let rec cob (a:int64) n =
    if n <> 0 then a * cob (a-1L) (n-1)
        else a    
for i in 1 .. T do
    let [|N;A;B|] = stdin.ReadLine().Split(' ') |> Array.map int64
    let lb = (N - A + 1L) * (N - A + 1L) % MOD
    let lr = (N-B+1L) * (N-B+1L) % MOD
    let m = lb * lr % MOD
    let higher = max A B
    let lower = min A B
    let diff = higher - lower
    let In = (diff+1L) * (diff+1L) % MOD
    if lower = 1L 
        then
            printfn "%d" ((m-In)%MOD)
        else
            let Out = 
                let mutable j = 0L
                for i in 1 .. lower-1L do
                    j <- (j + PowMOD (N-1L) (lower |> int) + cob N (lower|>int) % MOD 
                j
            printfn "%d" ((m-In-Out)%MOD)        
                    

                     





