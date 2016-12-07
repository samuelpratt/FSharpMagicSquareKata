module Program

//From http://stackoverflow.com/questions/3016139/help-me-to-explain-the-f-matrix-transpose-function
let rec transpose square =
    match square with
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let rec sum row = 
    match row with
        | [] -> 0
        | first::rest -> first + sum rest

//This isn't a good solution as it'll only work for 3x3 squares.
let sumIs15 row =
    if (sum row) = 15
    then true
    else false


let rec magicrows square =
    match square with
        | [] -> true
        | first::rest -> sumIs15 first && (magicrows rest)

let magiccolumns square = 
    let transSquare = transpose square
    magicrows transSquare

let rec size square =
    match square with
        | [] -> 0
        | first::rest -> 1 + size rest

let rec diagonal (square : int list list) x =
    match x with
        | 0 -> []
        | _ -> square.Item(x-1).Item(x-1) :: diagonal square (x-1)
   
let magicdiagonal square =
    let x = size square
    let diag = diagonal square x
    sumIs15 diag 

let magicdiagonals square =
    magicdiagonal square && magicdiagonal (transpose square)

let magicsquare square = 
    magicrows square
    && magiccolumns square
    && magicdiagonals square

[<EntryPoint>]
let main argv = 
    //This is a valid magic square and so this should return true.
    let square = [ [8; 1; 6;]; [3; 5; 7;]; [4; 9; 2]; ]
    let magic = magicsquare square
    match magic with
    | true -> printf "This is a magic square\n"
    | _ -> printf "This is not a magic square\n"

    0 


