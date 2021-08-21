// DPなんだろうなくらいで終了
let toNum c = 
    int(c) - int('A')
let MOD = 998244353L
let N = stdin.ReadLine() |> int
let str = stdin.ReadLine()
let contests = Array.map toNum (str.ToCharArray())
let serected = [|for i in 0..9 -> 0|]
let counts = [|for i in 0..9 -> 1L|]

let mutable ans = 0L

let rec calc index prev cnt = 
    if index = N then ()
    else
        let na = (ans + ans - serected.[contests.[index]] + 1L) % MOD
        let sa = 
        counts.[contests.[index]] <- 1L + counts.[[contests.[index]]
        serected.[contests.[index]] <- serected.[contests.[index]] + na
        calc (index + 1) (contets.[index]) ()

 