(********************************************************
* Arbogen-lib : fast uniform random generation of trees *
*********************************************************
* Module: Ast                                           *
* -------                                               *
* The Abstract syntax tree of the grammar               *
* -------                                               *
* (C) 2011, Xuming Zhan, Frederic Peschanski            *
*           Antonine Genitrini, Matthieu Dien           *
*           Marwan Ghanem                               *
*           under the                                   *
*           GNU GPL v.3 licence (cf. LICENSE file)      *
*********************************************************)


(* Grammar *)
type elem = Seq of string | Elem of string

type component = int option * elem option list

type rule = string * component list

type grammar = rule list

(* Parameter *)
type value = Vint of int | Vfloat of float | Vstring of string

type parameter = Param of string * value

(* Ast *)
type ast = parameter list * grammar


(* Ast.grammar to Grammar.grammar *)
let grm_elem_of_ast_elem = function
  | Seq s -> Grammar.Seq s
  | Elem s -> Grammar.Elem s

let grm_comp_of_ast_comp (w,comp) =
  match w with
  | None ->
    begin
	    match comp with
	    | [] -> failwith "You should not be here"
	    | [ elt ] ->
        begin
          match elt with
		      | Some (Seq _ as elt) -> Grammar.Cons (0, [grm_elem_of_ast_elem elt])
		      | Some (Elem e) -> Grammar.Call e
		      | _ -> failwith "You should not be here"
        end
	    | _ -> Grammar.Cons (0, (List.fold_left
                                 (fun cons_list elt ->
                                   match elt with
					                         | None -> cons_list
					                         | Some e -> (grm_elem_of_ast_elem e)::cons_list)
                                 []
                                 comp))
    end
  | Some n -> Grammar.Cons (n, (List.fold_left
                                  (fun cons_list elt ->
                                    match elt with
					                          | None -> cons_list
					                          | Some e -> (grm_elem_of_ast_elem e)::cons_list)
                                  []
                                  comp))

let grm_rule_of_ast_rule (name,comps) =
  (name, List.map grm_comp_of_ast_comp comps)

let grammar_of_ast_grammar ast_grammar =
  Grammar.completion (List.map grm_rule_of_ast_rule ast_grammar)

  
