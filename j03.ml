module Map : sig
  type t = private { width : int; height : int; map : bool array array }
  val read : unit -> t
  val tree : t -> int -> int -> bool
end = struct
  type t = { width : int; height : int; map : bool array array }

  let tree m x y = m.map.(y).(x mod m.width)

  let line_to_row width l =
    let row = Array.make width false in
    for i = 0 to width - 1 do
      row.(i) <- match l.[i] with
      | '.' -> false
      | '#' -> true
      | _ -> raise (Invalid_argument "incorrect input")
    done;
    row

  let read () =
    try
      let first = read_line () in
      let width = String.length first in
      let first = line_to_row width first in
      let rec loop acc =
        try loop (line_to_row width (read_line ()) :: acc)
        with End_of_file -> acc
      in
      let rows = loop [first] in
      let height = List.length rows in
      { width; height; map = Array.of_list rows }
    with End_of_file ->
      raise (Invalid_argument "empty input")
end

let check (dx,dy) map =
  let rec loop x y i =
    if y < 0 then i
    else loop (x+dx) (y+dy) (i + (if Map.tree map x y then 1 else 0))
  in
  let res = loop 0 (map.height-1) 0 in
  Printf.printf "Right %d, down %d: %d\n" dx (-dy) res;
  res

let () =
  let m = Map.read () in
  let x = check (1,-1) m in
  let y = check (3,-1) m in
  let z = check (5,-1) m in
  let t = check (7,-1) m in
  let u = check (1,-2) m in
  Printf.printf "Result: %d" @@ x*y*z*t*u
