[<Class>]
type Scan ()= 
    let mutable s = [||]
    let mutable l = 0
    let mutable i = 0

    let next() = 
        if i < l 
            then
                i <- i + 1 
                s.[i-1]
            else // 行が変わる時
                s <- stdin.ReadLine().Split(' ')
                i <- 1
                l <- s.Length
                s.[0]

    member this.nextInt = next() |> int
    member this.nextIntArray n = [|for i in 0 .. (n-1) -> next() |> int|]
    member this.nextInt64 = next() |> int64
    member this.nextInt64Array n = [|for i in 0 .. (n-1) -> next() |> int64|]
    member this.nextFloat = next() |> float
    member this.nextFloatArray n = [|for i in 0 .. (n-1) -> next() |> float|]
    member this.nextDouble = next() |> double
    member this.nextDoubletArray n = [|for i in 0 .. (n-1) -> next() |> double|]
    member this.nextChar = next() |> char 
    member this.nextCharArray = next().ToCharArray()
    member this.nextStr = next()
    member this.nextStrArray n = [|for i in 0 .. (n-1) -> next()|]



    //ローカルデバッグ用 参考　https://qiita.com/nia_tn1012/items/0bad76d75161ecc51e15
let exStdIn = new System.IO.StreamReader( "test.txt" )
exStdIn |> System.Console.SetIn