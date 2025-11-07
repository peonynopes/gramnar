type markup =
  | String of string

type count =
  | Integer of int
  | Infinity

type repetition = {
  minimum: count;
  maximum: count;
  greedy: bool
}

type regex =
  | Literal of string
  | Repeat of repetition * regex
  | Class of character_class
  | Predefined of predefined_class
and character_class = {
  negated: bool;
  items: class_item list
}
and class_item =
  | Range of string * string
  | Literal of string
  | Predefined of predefined_class
and predefined_class = {
  negated: bool;
  set: predefined_class_set
}
and predefined_class_set =
  | Word
  | Digit
  | Whitespace

type recipe =
  | Make_token
  | Make_tree of Name.component list

type metadata = {
  annotation: markup;
  action: recipe
}

type expansion =
  | Empty
  | Nonterminal of Name.name
  | Terminal of terminal
  | Or of expansion * expansion
  | Concatenation of expansion * expansion
  | Repeat of repetition * expansion
and terminal =
  | Regex of regex
  | Name of Name.name

type alternative = (expansion * metadata option)
type rhs = alternative list

type production = {
  id: Name.id;
  full_name: Name.name;
  rhs: (expansion * metadata option) list;
  annotation: markup (* annotation on the nonterminal *)
}

type grammar = {
  id: Name.id;
  contents: contents
}
and contents =
  | Lexical of production list
  | Phrase_structure of production list

type gramnar_module = {
  file_name: string;
  grammars: grammar list
  (* other stuff will go here *)
}

type project = {
  modules: gramnar_module list
}

let lookup_file (s : string) (p : project) : gramnar_module option =
  List.find_opt (fun ({file_name; _} : gramnar_module) -> file_name = s) p.modules

let lookup_grammar (grammar_name : Name.id) (gs : grammar list) : grammar option =
  List.find_opt (fun ({id; _} : grammar) -> id = grammar_name) gs

let lookup_symbol (symbol_name : Name.id) (ps : production list) : production option =
  List.find_opt (fun ({id; _} : production) -> id = symbol_name) ps

let lookup_name Name.{file_name; grammar_name; symbol_name} (p : project) : production option =
  match lookup_file file_name p with
  | Some {file_name = _; grammars} ->
    (match lookup_grammar grammar_name grammars with
     | Some {id = _; contents} -> (match contents with
       | Lexical ps | Phrase_structure ps -> lookup_symbol symbol_name ps)
     | None -> None)
  | None -> None

let empty_markup = String ""