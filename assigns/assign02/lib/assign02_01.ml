type piece = 
| X
| O
type pos = 
| Piece of piece
| Blank
type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)
type row_index = 
| Top
| Middle
| Bottom
type col_index = 
| Left
| Middle
| Right
type pos_index = row_index * col_index

let get_pos (board:board) (input:pos_index) : pos = 
    let (rowI, colI) = input in
    let row = match rowI with
    | Top -> let (r,_,_) = board in r
    | Middle -> let (_,r,_) = board in r
    | Bottom -> let (_,_,r) = board in r
    in
    match row with
    | (p1, p2, p3) -> match colI with
        | Left -> p1
        | Middle -> p2
        | Right -> p3

let win (a:pos) (b:pos) (c:pos) : bool =
    match a,b,c with
    | Piece X, Piece X, Piece X -> true
    | Piece O, Piece O, Piece O -> true
    | _ -> false
    
let winner (board: board) : bool =
    let row_win =
        win (get_pos board (Top, Left)) (get_pos board (Top, Middle)) (get_pos board (Top, Right)) ||
        win (get_pos board (Middle, Left)) (get_pos board (Middle, Middle)) (get_pos board (Middle, Right)) ||
        win (get_pos board (Bottom, Left)) (get_pos board (Bottom, Middle)) (get_pos board (Bottom, Right)) 
    in
    let col_win =
        win (get_pos board (Top, Left)) (get_pos board (Middle, Left)) (get_pos board (Bottom, Left)) ||
        win (get_pos board (Top, Middle)) (get_pos board (Middle, Middle)) (get_pos board (Bottom, Middle)) ||
        win (get_pos board (Top, Right)) (get_pos board (Middle, Right)) (get_pos board (Bottom, Right)) 
    in
    let diag_win =
        win (get_pos board (Top, Left)) (get_pos board (Middle, Middle)) (get_pos board (Bottom, Right)) ||
        win (get_pos board (Top, Right)) (get_pos board (Middle, Middle)) (get_pos board (Bottom, Left)) 
    in row_win || col_win || diag_win