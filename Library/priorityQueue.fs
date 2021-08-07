// 昇順
// 降順にするときは　cmd+F -> /// のところの比較演算子を変える
// 扱う型を変えるときは　Arr:int[]  の　intを変える

[<Class>]
type PriorityQueue (N) =
    let mutable (Arr:int[]) = [|1 .. N|]
    let mutable i = 0
    let l = Arr.Length

    let Swap a b = 
        let s = Arr.[a]
        Arr.[a] <- Arr.[b]
        Arr.[b] <- s

    let rec ParentCompare par num =
        if num = 0 then ()
            else if Arr.[par] > Arr.[num] ///ここを変える
                then 
                    Swap par num
                    ParentCompare ((par - 1) / 2) par

    let child num = 
        let c = 2 * (num + 1)
        if c >= i || Arr.[c] > Arr.[c-1] then c-1 else c  ///

    let rec ChildCompare num = 
        if num < i then
            let c = child num
            if c < i then
                if Arr.[num] > Arr.[c] then ///ここを変える
                    Swap num c
                    ChildCompare c

    member this.Length = i
    member this.Array = Arr

    member this.Push value = 
        if i < l then
            Arr.[i] <- value
            if i <> 0 then
                let par = (i - 1) / 2
                let num = i
                ParentCompare par num
            i <- i+1  

    member this.Pull = 
        i <- i - 1
        if i > 0 then 
            Swap i 0
            ChildCompare 0
        Arr.[i]