
//コピペして PowNMod 底　指数　法　で計算

let rec culc sq v n Mod =
    if n = 0 then v
    else
        let next = n >>> 1
        let nv = if n &&& 1 = 1 then v * sq % Mod else v
        culc (sq * sq % Mod) nv next Mod

let PowNMod e n Mod = 
    culc e 1L n Mod