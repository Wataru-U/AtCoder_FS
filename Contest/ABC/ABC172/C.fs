let [|N;M;K|] = stdin.ReadLine().Split() |> Array.map int
let k = K |> int64
let A = stdin.ReadLine().Split() |> Array.map int64
let B = stdin.ReadLine().Split() |> Array.map int64

let mutable s = 0L
let mutable ai = 0

let rec ACalc ()= 
    if ai = N then ()
    else
        if s + A.[ai] > k then ()
        else
            s <- s + A.[ai]
            ai <- ai + 1
            ACalc ()

ACalc ()

let mutable ans = ai
let mutable bi = 0

let rec BCalc () = 
    if bi = M then ()
    elif s + B.[bi] > k then ()
    else
        s <- s + B.[bi]
        bi <- bi + 1
        BCalc()

let rec Calc () = 
    if ai = 0 then ()
    elif bi = M then ()
    else
        ai <- ai - 1
        s <- s - A.[ai]
        BCalc()
        ans <- max ans (ai + bi)
        Calc()
BCalc()

ans <- ans + bi
Calc()

printfn "%d" ans