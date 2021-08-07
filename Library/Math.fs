[<AbstractClass; Sealed>]
type Math private() =

    static let Module = 1000000007L
    static let rec pownPer (a:int64) n =
        let mutable r = 1L
        for i in 1 .. n do
            r <- (r * a) % Module
        r % Module 

 
    static let rec pn (v:int64) (lis:int[]) =
        if lis.Length = 0
            then true
            else
                let head = lis.[0] |> int64
                if head = v 
                    then true
                    else if v % head = 0L
                        then false
                        else pn v (lis |> Array.choose(fun x -> if (x|> int64) % head <> 0L then Some x else None))    

    // 合同式関係
    static member MOD = Module
    static member PownPerMod a n  = pownPer a n
    //素数判定
    static member PrimeNum (v:int64) = 
        let sqV = v |> float |> sqrt |> int
        printfn "%d" sqV
        let lis = [|2 .. sqV+1|]
        pn v lis


let n = stdin.ReadLine().Split() |> Array.map int64
for i in n do i |> Math.PrimeNum |> printfn "%b"
