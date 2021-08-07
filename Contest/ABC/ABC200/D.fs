let n = stdin.ReadLine() |> int
let A = stdin.ReadLine().Split() |> Array.map int
let mutable ans = [|for i in 0..199 -> []|]
let Min = min 8 n
for i in 1..(1 <<< Min - 1) do
    let mutable l = []
    let mutable sum = 0
    for j in 0..(Min-1) do
        if i>>>j &&& 1 = 1 then
            sum <- (sum + A.[j]) % 200
            l <- j :: l
    ans.[sum] <- l :: ans.[sum]
let Ans (v:list<int>) =
    printf "%d" v.Length
    for i in (v |> List.rev) do
        printf " %d" (i+1)
    printfn  ""
let rec culc idx = 
    if idx = 200 then   
        printfn "No"
    elif ans.[idx].Length >= 2 
    then
        printfn "Yes"
        Ans ans.[idx].[0]
        Ans ans.[idx].[1]
    else culc (idx+1)
culc 0