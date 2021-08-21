// pythonでやったので最後まで書いてない
let [|S;k|] = stdin.ReadLine().Split()
let K = k |> int
let pat = []
let rec v index c (str: string)=
    if str.Length = S.Length then pat <- str :: pat
    else
        