type min_max = {min: int; max: int}

let get_str_list_list str =
  let str_lst_lst = List.map(fun x -> String.split_on_char '\t' x) (String.split_on_char '\n' str) in
  List.map (fun x -> List.map (fun y -> int_of_string y) x) str_lst_lst

let find_min_max str_lst =
  let rec f acc lst =
    match lst with
    | [] -> acc
    | x :: xs ->
      if x > acc.max then f {min = acc.min; max = x} xs
      else if x < acc.min then f {min = x; max = acc.max} xs
      else f acc xs in
  match str_lst with
  | [] -> {min = 0; max = 0}
  | x :: xs -> f {min = x; max = x} xs

let get_diff str_lst =
  let find_result = find_min_max str_lst in
  find_result.max - find_result.min

let find_divisible int_lst =
  let preprocessed = List.filter (fun x -> x != 1) int_lst in
  let associated = List.map (fun x -> (x, preprocessed)) preprocessed in
  let ys_filtered = List.map (fun (x, ys) -> (x, List.filter (fun y -> x != y && (x mod y) = 0) ys)) associated in
  let filtered = List.filter
      (fun (_, ys) -> match ys with
         | [] -> false
         | _ -> true) ys_filtered in
  let max_elem = List.fold_left (fun (x, xs) (y, ys) -> if (x > y) then (x, xs) else (y, ys)) (0, []) filtered in
  match max_elem with
  | (_, []) -> 0
  | (x, y :: _) -> x / y
  

let get_answer find_func input =
  List.fold_left (fun acc next -> acc + next) 0
    (List.map (fun x -> find_func x) (get_str_list_list input))

let example_input_1 =
  "5\t1\t9\t5
7\t5\t3
2\t4\t6\t8"

let example_input_2 =
  "5\t9\t2\t8
9\t4\t7\t3
3\t8\t6\t5"

let question =
  "121	59	141	21	120	67	58	49	22	46	56	112	53	111	104	130
1926	1910	760	2055	28	2242	146	1485	163	976	1842	1982	137	1387	162	789
4088	258	2060	1014	4420	177	4159	194	2794	4673	4092	681	174	2924	170	3548
191	407	253	192	207	425	580	231	197	382	404	472	164	571	500	216
4700	1161	168	5398	5227	5119	252	2552	4887	5060	1152	3297	847	4525	220	262
2417	992	1445	184	554	2940	209	2574	2262	1911	2923	204	2273	2760	506	157
644	155	638	78	385	408	152	360	588	618	313	126	172	220	217	161
227	1047	117	500	1445	222	29	913	190	791	230	1281	1385	226	856	1380
436	46	141	545	122	86	283	124	249	511	347	502	168	468	117	94
2949	3286	2492	2145	1615	159	663	1158	154	939	166	2867	141	324	2862	641
1394	151	90	548	767	1572	150	913	141	1646	154	1351	1506	1510	707	400
646	178	1228	1229	270	167	161	1134	193	1312	1428	131	1457	719	1288	989
1108	1042	93	140	822	124	1037	1075	125	941	1125	298	136	94	135	711
112	2429	1987	2129	2557	1827	477	100	78	634	352	1637	588	77	1624	2500
514	218	209	185	197	137	393	555	588	569	710	537	48	309	519	138
1567	3246	4194	151	3112	903	1575	134	150	4184	3718	4077	180	4307	4097	1705"

let _ = 
  print_endline ("Result for First Question for Example Input: " ^ string_of_int (get_answer get_diff example_input_1));
  print_endline ("Result for First Question for Real Input: " ^ string_of_int (get_answer get_diff question));
  print_endline ("Result for Second Question for Example Input: " ^ string_of_int (get_answer find_divisible example_input_2));
  print_endline ("Result for Second Question for Real Input: " ^ string_of_int (get_answer find_divisible question));;
