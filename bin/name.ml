type id = (* bare identifier *)
  | User of string
  | Generated of int

type name = {
  file_name: string;
  grammar_name: id;
  symbol_name: id
}

type component =
  | Numeric of int
  | Named of name

type name_with_component = {
  name: name;
  component: component
}

let generate_name : name -> name =
  let next = ref 0 in
  fun {file_name; grammar_name; _} ->
    let generated = !next in
    incr next;
    {file_name; grammar_name; symbol_name = Generated generated}

let display_name {file_name; grammar_name; symbol_name} : string =
  let display_id = function
    | User s -> s
    | Generated i -> Printf.sprintf "_%d" i
  in Printf.sprintf "%s.%s.%s"
      file_name (display_id grammar_name) (display_id symbol_name)
