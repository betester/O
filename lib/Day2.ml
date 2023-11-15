type jajanken = Rock | Paper | Scissor

let map_jajanken = function
  | "A"  -> Rock 
  | "X" -> Rock
  | "B" -> Paper
  | "Y" -> Paper
  | "C"  -> Scissor
  | "Z" -> Scissor
  | _ -> Rock

let jajanken_score o1 o2 = 
  match o1 with
    | Rock -> 
      (match o2 with
        | Rock -> 4
        | Paper -> 1
        | Scissor -> 7)
    | Paper -> 
      (match o2 with
        | Rock -> 8
        | Paper -> 5
        | Scissor -> 2)
    | Scissor -> 
      match o2 with
        | Rock -> 3
        | Paper -> 9
        | Scissor -> 6

let rec battle = function
  | [] -> 0
  | x :: xs -> 
    (match x with 
      | o1 :: o2 :: _ -> 
        let map_o1 = map_jajanken o1 in
        let map_o2 = map_jajanken o2 in
        (jajanken_score map_o2 map_o1) + battle xs 
      | _ :: _ -> 0
      | [] -> 0
    ) 
    
let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs

let main () = 
  let plays = Reader.file_reader "./lib/day2.txt" in
  let map_plays = map (' ' |> String.split_on_char) plays in 
  let total_score = battle map_plays in
  print_int total_score


