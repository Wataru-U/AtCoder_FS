let Big a = 
    a = 'A' || a = 'B' || a = 'C' || a = 'D' ||  a = 'E' || a = 'F' || a = 'G' || a = 'H' || 
    a = 'I' || a = 'J' || a = 'K' || a = 'L' ||  a = 'M' || a = 'N' || a = 'O' || a = 'P' ||
    a = 'Q' || a = 'R' || a = 'S' || a = 'T' ||  a = 'U' || a = 'V' || a = 'W' || a = 'X' ||
    a = 'Y' || a = 'Z'

let mutable ans  = "Yes"
stdin.ReadLine()
|> fun x -> 
    for i in  0 .. (x.Length-1) do
        if (i % 2 = 0 && Big x.[i]) || (i % 2 = 1 && (Big x.[i]) = false)
            then ans <- "No"

printfn "%s" ans
