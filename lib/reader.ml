let file_reader filepath =
  let channel = open_in filepath in
  try
   let rec read_lines lines = 
    try
     let line = input_line channel in
     read_lines (line :: lines)
    with End_of_file ->
     close_in channel;
     List.rev lines in
   read_lines []
  with e ->
   close_in channel;
   raise e
