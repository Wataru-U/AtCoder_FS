open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Text



// -- AutoFlufh Off -- //

new StreamWriter(Console.OpenStandardOutput())
|> fun x -> x.AutoFlush <- false; x
|> Console.SetOut

type Edge =
        val To : int
        val Cost : int64
        new(i,c) = {To = i;Cost = c}

for pat in 0 .. 99 do

    let name = "AHC005/in/" + (if pat > 9 then "00" else "000") + (pat |> string) + ".txt"
    printfn "cace is %d" pat
    Console.Out.Flush()
    let exStdIn = new System.IO.StreamReader( name )
    exStdIn |> System.Console.SetIn


    // ---- input & variables ---- //

    let [|N;Si;Sj|] = stdin.ReadLine().Split() |> Array.map int

    let Map = 
        [|
            for i in 1..N -> 
                stdin.ReadLine().ToCharArray()
                |> Array.map (fun x -> if x = '#' then -1 else x |> string |> int )
        |]

    // 他の道とぶつかる所　　スタートを含む
    let mutable (crossRoads: (int * int) List) = new List<int * int>()

    let mutable Indexes = Array.init N (fun _ -> Array.init N (fun _ -> -1))
    let mutable (To: Edge list []) = [||]
    let mutable startIndex = 0

    // ---- Lib ---- //

    let tfst x = x |> (fun (a,_,_) -> a)
    let tsnd x = x |> (fun (_,a,_) -> a)
    let tthd x = x |> (fun (_,_,a) -> a)

    let Move x y = 
        let mutable r = []
        do
            if x <> 0
            then if Map.[x-1].[y] <> -1 then r <- "U" :: r
            if x <> N-1 
            then if Map.[x+1].[y] <> -1 then r <- "D" :: r
            if y <> 0
            then if Map.[x].[y-1] <> -1 then r <- "L" :: r
            if y <> N-1 
            then if Map.[x].[y+1] <> -1 then r <- "R" :: r
        r

    //　道を辿って交差するところを探す
    let rec flow x y (dir:int[]) cost = 
        if x < 0 || x = N || y < 0 || y = N then (-1,-1,-1)
        elif Map.[x].[y] = -1 then (-1,-1,-1)
        elif Indexes.[x].[y] <> -1 then (x,y,cost) 
        else flow (x + dir.[0]) (y + dir.[1]) dir (cost + Map.[x].[y])

    let rec makeRoute index = 
        if To.[index].Length <> 0 then ()
        else
            do
                for i in [|[|-1;0|];[|1;0|];[|0;-1|];[|0;1|]|] do
                    let CR = crossRoads.[index]
                    let nextCR = flow (fst CR + i.[0]) (snd CR + i.[1]) i 0
                    if nextCR <> (-1,-1,-1) 
                    then
                        let nextIndex = Indexes.[tfst nextCR].[tsnd nextCR] 
                        To.[index] <- Edge(nextIndex, tthd nextCR |> int64) :: To.[index]
                        makeRoute nextIndex
    
    let cross (x : string list) =
        let toNum s =
            match s with 
            | "U" | "D" -> 1
            | _ -> 2
        toNum x.[0] <> toNum x.[1]

    //　とりあえず　ある程度の交差点を DFS　で回る
    let rec dfs index (seen : bool []) =
        let mutable ns = seen
        ns.[index] <- false
        let CR = crossRoads.[index]
        if index = startIndex 
        then
            printfn "%d %d" index To.[index].Length
            Console.Out.Flush()
            let nextCR = crossRoads.[To.[index].[0].To]
            let xDiff = fst nextCR - fst CR
            let yDiff = snd nextCR - snd CR
            let dir = 
                if xDiff > 0 then "D" elif xDiff < 0 then "U"
                elif yDiff > 0 then "R" else "L"
            let num = xDiff + yDiff |> abs
            for i in 1..num do
                printf "%s" dir
            dfs To.[index].[0].To seen 
            let rev = 
                match dir with
                | "U" -> "D"
                | "D" -> "U"
                | "L" -> "R"
                | _ -> "L"
            for i in 1..num do
                printf "%s" rev
        else
        do
            for i in To.[index] do
                printfn "%d %d" i.To seen.Length
                if seen.[i.To] then
                    let nextCR = crossRoads.[i.To]
                    let xDiff = fst nextCR - fst CR
                    let yDiff = snd nextCR - snd CR
                    let dir = 
                        if xDiff > 0 then "D" elif xDiff < 0 then "U"
                        elif yDiff > 0 then "R" else "L"
                    let num = xDiff + yDiff |> abs
                    for j in 1..num do
                        printf "%s" dir
                    dfs i.To seen 
                    let rev = 
                        match dir with
                        | "U" -> "D"
                        | "D" -> "U"
                        | "L" -> "R"
                        | _ -> "L"
                    for j in 1..num do
                        printf "%s" rev


        


    // ---- preprocessing ---- //
    printfn "before"
    for i in 0..N-1 do
        for j in 0..N-1 do
            if Map.[i].[j] <> -1 
            then 
                let movalbe = Move i j
                if movalbe.Length > 2 || (i = Si && j = Sj)
                then 
                    Indexes.[i].[j] <- crossRoads.Count
                    if i = Si && j = Sj then startIndex <- crossRoads.Count
                    crossRoads.Add (i,j)
                elif movalbe.Length = 2
                then
                    if cross movalbe 
                    then
                        Indexes.[i].[j] <- crossRoads.Count
                    if i = Si && j = Sj then startIndex <- crossRoads.Count
                    crossRoads.Add (i,j)
                    
    printfn "prepro"
    Console.Out.Flush()
    To <- Array.init crossRoads.Count (fun _ -> [])

    makeRoute 0

    printfn "str %d l %d " startIndex crossRoads.Count
    
    let mutable seen = Array.init  crossRoads.Count (fun _ -> true)
    seen.[startIndex] <- true
    printfn "before dfs"
    Console.Out.Flush()
    dfs startIndex seen

Console.Out.Flush()

