let n = stdin.ReadLine() |> int
let mutable v = [|for i in 1 .. n -> (i*2%n+1,(i*2+1)%n+1)|]


let rec set a b=
    if b-a <= 2
        then 
            v.[a] <- (0,n-1)
            ()
        else 
            let index = (a+b) / 2
            let uIndex = (a + index) / 2
            let bIndex = (b + index) / 2
            set a index
            set index b
            v.[index] <- (uIndex,bIndex)
            ()


set 0 (n-1)

v.[0] <- (0,(n-1)/2)
v.[n-1] <- (0,(n-1)/2)
if n = 2 then v.[0] <- (0,1)

for i in v do 
    let a = i |> (fun (x,y) -> x+1)
    let b = i |> (fun (x,y) -> y+1)
    printfn "%d %d" a b
