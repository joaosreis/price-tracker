type t = NoStock | Stock of float

let equal x y = match x,y with
    NoStock, NoStock -> true
    | NoStock, Stock _
    | Stock _, NoStock -> false
    | Stock p1, Stock p2 -> abs_float (p1 -. p2) < 0.01
