open Utils

let lex s =
    let words = split s in
        let rec convert words acc =
            match words with
            | [] -> Some (List.rev acc)
            |word :: rest -> (
                match tok_of_string_opt word with
                | Some token -> convert rest (token :: acc)
                | None -> None
            )
    in convert words []