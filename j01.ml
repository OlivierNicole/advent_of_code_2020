let cpt = ref 0

module List = struct
  include List
  let rec find p l =
    match l with
    | [] -> raise Not_found
    | x :: xs ->
        incr cpt;
        if p x then x else find p xs
end
(* Part One *)

let rec find_two_adding_upto n l =
  match l with
  | x :: xs when x < 2020 ->
    begin
      try
        let y = List.find (fun y -> x + y = n) xs in
        x, y, x * y
      with Not_found -> find_two_adding_upto n xs
    end
  | _ -> raise Not_found

(* Part Two *)

let rec find_three_adding_upto n l =
  match l with
  | [] | [_] -> raise Not_found
  | x :: xs ->
      try
        let y,z,_ = find_two_adding_upto (2020-x) xs in
        x,y,z, x*y*z
      with Not_found -> find_three_adding_upto n xs

let () =
  let rec loop acc =
    try match int_of_string_opt (read_line ()) with
      | Some x -> loop (x :: acc)
      | None -> acc
    with End_of_file -> acc
  in
  let input = List.rev (loop []) in
  let x,y,p = find_two_adding_upto 2020 input in
  Printf.printf "%d * %d = %d\n" x y p;
  Printf.printf "cpt = %d\n" !cpt;
  cpt := 0;
  let x,y,z,p = find_three_adding_upto 2020 input in
  Printf.printf "%d * %d * %d = %d\n" x y z p;
  Printf.printf "cpt = %d\n" !cpt;
