type policy = { char : char; lb : int; ub : int }

let parse str =
  Scanf.sscanf str "%d-%d %c: %s" (fun lb ub c s ->
    { lb; ub; char = c }, s
  )

let count c s =
  let i = ref 0 in
  s |> String.iter (fun c' -> if c = c' then incr i);
  !i

let complies policy s =
  let c = count policy.char s in
  policy.lb <= c && c <= policy.ub

let complies_part_two policy s =
  let c1 = try s.[policy.lb-1] = policy.char with Invalid_argument _ -> false in
  let c2 = try s.[policy.ub-1] = policy.char with Invalid_argument _ -> false in
  (c1 && not c2) || (not c1 && c2)

let fold_lines init (f : string -> 'a -> 'a) : 'a =
  let rec loop acc =
    try loop @@ f (read_line ()) acc
    with End_of_file -> acc
  in
  loop init

let () =
  let n1,n2 = fold_lines (0,0) (fun s (n1,n2) ->
    let policy, s = parse s in
    let n1 =
      if complies policy s then n1+1 else n1
    in
    let n2 = if complies_part_two policy s then n2+1 else n2 in
    n1,n2
  ) in
  Printf.printf "Result part one: %d\nResult part two: %d\n" n1 n2
