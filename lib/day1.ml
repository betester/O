

let rec max_calories list_of_reindeers current_sum = 
  match list_of_reindeers with
  | [] -> min_int
  | x :: xs ->
     match x with
     | "" -> max current_sum (max_calories xs 0) 
     | _ ->
       let int_of_x = int_of_string x in
       max_calories xs (current_sum + int_of_x)


let main () = 
  let list_of_reindeer = Reader.file_reader "./lib/day1.txt" in
  print_int (max_calories list_of_reindeer 0)
