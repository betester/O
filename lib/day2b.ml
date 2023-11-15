type jajanken = Rock | Paper | Scissor 
type action = Win | Draw | Lose

let map_jajanken = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissor
    | _ -> Rock

let map_action = function
    | "X" -> Win
    | "Y" -> Draw
    | "Z" -> Lose
    | _ -> Win

let jajanken_score o1 o2 = 
  match o1 with
    | Rock -> 
      (match o2 with 
        | Win -> 8  
        | Draw -> 4
        | Lose -> 3)
    | Paper -> 
      (match o2 with 
        | Win -> 9
        | Draw -> 5
        | Lose -> 1
      )
    | Scissor -> 
      (match o2 with 
        | Win -> 7
        | Draw -> 6
        | Lose -> 2
      )

let rec battle = function
  | [] -> 0
  | x :: xs ->   
    (match x with
      | o1 :: o2 :: _ ->
        let opponent_jajanken = map_jajanken o1 in
        let our_action = map_action o2 in
        (jajanken_score opponent_jajanken our_action) + battle xs 
      | _ :: _ -> 0
      | [] -> 0
    )

let main () = 
  let jajanken_actions = Reader.file_reader "./lib/day2.txt" in
  let mapped_actions = List.map (' ' |> String.split_on_char) jajanken_actions in 
  let total_score = battle mapped_actions in
  print_int total_score

