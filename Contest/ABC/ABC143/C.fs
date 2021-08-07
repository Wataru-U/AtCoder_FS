let N = stdin.ReadLine() |> int

let s = stdin.ReadLine()

let rec calc index v = 
    if index = N then v
    elif s.[index] = s.[index-1] then calc (index+1) (v-1)
    else calc (index+1) v

calc 1 N