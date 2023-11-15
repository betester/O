
let rec max_calories list_of_reindeers current_sum = 
  match list_of_reindeers with
  | [] -> [] 
  | x :: xs ->
     match x with
     | "" -> current_sum :: max_calories xs 0 
     | _ ->
       let int_of_x = int_of_string x in
       max_calories xs (current_sum + int_of_x)

let rec filter f = function
  | [] -> []
  | x :: xs -> 
    if f x 
     then x :: filter f (xs)
    else 
      filter f xs

let rec quick_sort = function
  | [] -> []
  | x :: xs -> 
    let left_side = filter (fun y -> y <= x) xs in
    let right_side = filter (fun y -> y > x) xs in
    (quick_sort right_side) @ [x] @ (quick_sort left_side) 

let main () = 
  let list_of_reindeer = Reader.file_reader "./lib/day1.txt" in
  let sorted_reindeer = 0 |> max_calories list_of_reindeer |> quick_sort in 
  let top_three_sum = match sorted_reindeer with
    | [] -> 0  
    | x :: y :: z :: _ -> x + y + z
    | x :: y :: _ -> x + y
    | x :: _ -> x
  in 
  print_int top_three_sum
