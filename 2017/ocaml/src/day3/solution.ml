type corner_nums = {lt: float; rt: float; rb: float; lb: float}
type distance_info = {corner_num: float; dist: float}

let less_than_square target to_be_squared = target <= (to_be_squared ** 2.0)

let find_max_num_sqrt target =
  let rec f to_compare =
    if less_than_square target to_compare then to_compare
    else f (to_compare +. 2.0) in
  f 1.

let find_corner_nums max_num_sqrt =
  let start_num = (max_num_sqrt -. 2.0) ** 2.0 in
  let rt = start_num +. max_num_sqrt -. 1.0 in
  let lt = rt +. max_num_sqrt -. 1.0 in
  let lb = lt +. max_num_sqrt -. 1.0 in
  let rb = max_num_sqrt ** 2.0 in
  {lt = lt; rt = rt; rb = rb; lb = lb}

let find_closest_corner target_num corners max_num_sqrt =
  let to_subtract = (max_num_sqrt -. 1.0) /. 2.0 in
  let corner_lst = [corners.lt; corners.rt; corners.rb; corners.lb] in
  let side_centers: float list = List.map (fun x -> x -. to_subtract) corner_lst in
  List.fold_left
    (fun acc x -> if abs_float(x -. target_num) < acc.dist then {corner_num = x; dist = abs_float(x -. target_num)} else acc)
    {corner_num = 0.0; dist =  infinity}
    side_centers

let find_distance target_num =
  if target_num = 1.0 then 0.0 else
  let max_num_sqrt = find_max_num_sqrt target_num in
  let corner_nums = find_corner_nums max_num_sqrt in
  if target_num == corner_nums.rb ||
     target_num == corner_nums.rt ||
     target_num == corner_nums.lt ||
     target_num == corner_nums.lb then
    max_num_sqrt -. 1.0
  else
    let dist_info = find_closest_corner target_num corner_nums max_num_sqrt in
    dist_info.dist +. ((max_num_sqrt -. 1.0) /. 2.0)

let _ =
  print_endline ("Answer for 1 is " ^ string_of_float(find_distance 1.0));
  print_endline ("Answer for 12 is " ^ string_of_float(find_distance 12.0));
  print_endline ("Answer for 23 is " ^ string_of_float(find_distance 23.0));
  print_endline ("Answer for 1024 is " ^ string_of_float(find_distance 1024.0));
  print_endline ("Answer for 289326 is " ^ string_of_float(find_distance 289326.0));
