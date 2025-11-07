module G = Grammar

let map_grammar (f : G.production -> G.production list) (g : G.grammar) : G.grammar =
  match g with
  | {id; contents = Lexical ps} ->
      {id; contents = Lexical (List.concat_map f ps)}
  | {id; contents = Phrase_structure ps} ->
      {id; contents = Phrase_structure (List.concat_map f ps)}

let map_alternatives (f : G.alternative -> G.production -> G.alternative * G.production list) (g : G.grammar) : G.grammar =
  let go (G.{id; full_name; rhs; annotation} as production) =
    let (alternatives, new_productionses) = List.split (List.map (fun a -> f a production) rhs)
    in G.{id; full_name; rhs = alternatives; annotation} :: (List.flatten new_productionses)
  in map_grammar go g

let rec flatten_disjunction = function
  | G.Or (e1, e2) -> flatten_disjunction e1 @ flatten_disjunction e2
  | x -> [x]

let remove_inner_disjunctions (g : G.grammar) =
  let rec go (expansion, m) (G.{full_name; _} as p : G.production) = match expansion with
    | G.Nonterminal _ | G.Terminal _ | G.Empty -> ((expansion, m), [])
    | G.Or (e1, e2) ->
        let flattened = (flatten_disjunction e1) @ (flatten_disjunction e2) in
        let (Name.{symbol_name; _} as name) = Name.generate_name full_name in
          ((G.Nonterminal name, m),
           [G.{id = symbol_name;
               full_name = name;
               rhs = List.map (fun e -> (e, None)) flattened;
               annotation = G.empty_markup}])
    | G.Concatenation (e1, e2) ->
        let ((e1a, _), new_productions) = go (e1, m) p in
        let ((e2a, _), new_productions2) = go (e2, m) p in
          ((G.Concatenation (e1a, e2a), m), new_productions @ new_productions2)
    | G.Repeat (r, e) ->
        let ((e1, _), new_productions) = go (e, m) p in
          ((G.Repeat (r, e1), m), new_productions)
  in map_alternatives go g

let rec make_fixed_length_string (s : G.expansion) = function
  | 0 -> G.Empty (* only if 0 was passed *)
  | 1 -> s (* this is the base case *)
  | n -> G.Concatenation (s, make_fixed_length_string s (n - 1))

let remove_repetitions (g : G.grammar) : G.grammar =
  let rec go (expansion, m) (G.{full_name; _} as p : G.production) = match expansion with
  | G.Nonterminal _ | G.Terminal _ | G.Empty -> ((expansion, m), [])
  | G.Or (e1, e2) ->
      let ((e1a, _), new_productions) = go (e1, m) p in
      let ((e2a, _), new_productions2) = go (e2, m) p in
       ((G.Or (e1a, e2a), m), new_productions @ new_productions2)
  | G.Concatenation (e1, e2) ->
      let ((e1a, _), new_productions) = go (e1, m) p in
      let ((e2a, _), new_productions2) = go (e2, m) p in
        ((G.Concatenation (e1a, e2a), m), new_productions @ new_productions2)
  | G.Repeat ({minimum = (Integer minimum); maximum; _}, e) ->
      let e_name = Name.generate_name full_name in
      let e_nonterminal = G.Nonterminal e_name in
      let e_repeat_name = Name.generate_name full_name in
      let e_repeat = G.Nonterminal e_repeat_name in
      let make_rule (Name.{symbol_name; _} as name) rhs =
        G.{id = symbol_name; full_name = name; rhs; annotation = G.empty_markup} in
      let minimum_alternative = make_fixed_length_string e_nonterminal minimum, None in
      let e_repeat_rule = make_rule e_repeat_name (match maximum with
                   | Infinity -> [minimum_alternative; Concatenation (e_repeat, e_nonterminal), None]
                   | Integer n ->
                       minimum_alternative
                       :: (List.init n (fun i ->
                                         (make_fixed_length_string e_nonterminal i, None))))
      in ((G.Nonterminal e_repeat_name, m),
          [make_rule e_name [e, None];
           e_repeat_rule])
  | G.Repeat (_, _) -> failwith "bad repetition"
  in map_alternatives go g

let test () =
  let name n = Name.{file_name = "test"; grammar_name = User "test"; symbol_name = User n} in
  let prod n rhs = G.{id = User n;
                      full_name = name n;
                      rhs = rhs;
                      annotation = G.empty_markup} in
  let alt expansion = (expansion, None) in
  let term_a = G.Terminal (Name (name "a")) in
  let term_b = G.Terminal (Name (name "b")) in
  let term_c = G.Terminal (Name (name "c")) in
  let nonterm_s = G.Nonterminal (name "S") in
  let result = remove_inner_disjunctions G.{
    id = Name.User "test";
    contents = Phrase_structure [
                prod "S" [alt term_a;
                          alt (Concatenation
                                (term_a,
                                 (Concatenation
                                   (Or (term_a,
                                        (Or ((Repeat ({minimum = Integer 3; maximum = Infinity; greedy = true}, term_b)),
                                             term_c))),
                                    nonterm_s))))]
               ]
  }
  in G.print_grammar (remove_repetitions result)