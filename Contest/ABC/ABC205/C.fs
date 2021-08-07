let [|A;B;C|] = stdin.ReadLine().Split() |> Array.map int
let a = abs A
let b = abs B
if A = B then "="
elif A >= 0 && B >= 0 then
    if A > B then ">"
    else "<"
elif A < 0 && B < 0 then
    if C % 2 = 0 then
        if a > b then ">"
        elif A = B then "="
        else "<"
    else
        if A > B then ">"
        else "<"
elif C % 2 = 0 then
    if a > b then ">"
    elif a = b then "="
    else "<"
elif A < 0 then "<"
else ">"
|> printfn "%s"


        