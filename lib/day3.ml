let rec len = function
  | [] -> 0
  | _ :: xs -> 1 + len xs

let rec sub a b = function
  | [] -> []
  | x :: xs -> 
      if a = 0 then
        if b < 0 then []
        else x :: sub a (b-1) xs
      else sub (a-1) (b-1) xs 

let rec same_number a1 a2 =  
  match a1 with
    | [] -> -1
    | x :: xs -> 
      (
       match a2 with
        | [] -> -1 
        | y :: ys ->
        if y == x then 
         same_number xs ys |> max x 
        else if y > x then
          same_number xs a2 
        else
          same_number a1 ys  
    )

let rec quicksort = function
  | [] -> []
  | x :: xs -> 
    let left_side = List.filter (fun y -> y < x) xs in
    let right_side = List.filter (fun y -> y > x) xs in 
    quicksort left_side @ [x] @ quicksort right_side


let to_list_of_int list = 
  let uppercase_to_int a = 
    let char_repr = int_of_char a in
    if char_repr >= 97 then char_repr - 96
    else char_repr - 64 + 26 
  in
  List.map uppercase_to_int list


let list_of_string s =
  let rec aux index acc =
    if index < 0 then
      acc
    else
      aux (index - 1) (s.[index] :: acc)
  in
  aux (String.length s - 1) []

let max_compartment_item list = 
  let n = (len list)/2 in
  let left_half = list |> sub 0 (n - 1) |> quicksort in
  let right_side = list|> sub n (n*2) |> quicksort in
  same_number left_half right_side

let main () = 
  let compartments = Reader.file_reader "./lib/day3.txt" in 
  let compartment_mapper list = list |> list_of_string |> to_list_of_int in 
  let mapped_compartment = List.map compartment_mapper compartments in 
  let compartment_items = List.map max_compartment_item mapped_compartment in 
  print_int (List.fold_left (+) 0 compartment_items)
