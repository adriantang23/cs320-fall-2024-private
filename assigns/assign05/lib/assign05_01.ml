type 'a test = 
| TestCase of 'a
| TestList of 'a test list

let rec fold_left op base test =
    match test with
    | TestCase x -> op base x
    | TestList y -> List.fold_left (fold_left op) base y