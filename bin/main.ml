open Sexplib

module OperationMap = Map.Make(String)

type context = {
  operations: (string list -> string) OperationMap.t 
}

let rec eval ctx r sexp =
  match sexp with
  | Sexp.Atom s -> s
  | Sexp.List [] -> r
  | Sexp.List (Atom op :: args) -> (OperationMap.find op ctx.operations) (List.map (eval ctx r) args)
  | _ -> raise (Failure "syntax error")

let add args =
  List.map int_of_string args |> List.fold_left (+) 0 |> string_of_int

let sub args =
  let r = match List.map int_of_string args with
  | [] -> 0
  | [n] -> n * -1
  | h :: tl -> List.fold_left (-) h tl
  in
  string_of_int r

let mul args =
  List.map int_of_string args |> List.fold_left ( * ) 1 |> string_of_int

let div args =
  let r = match List.map int_of_string args with
  | [] -> raise (Failure "division must have at least one argument")
  | [n] -> 1 / n
  | h :: tl -> List.fold_left (/) h tl
  in
  string_of_int r
    

let default_ops_map = OperationMap.empty
  |> OperationMap.add "+" add
  |> OperationMap.add "-" sub
  |> OperationMap.add "*" mul
  |> OperationMap.add "/" div

let () =
  let code = Sys.argv.(1) in
  let sexp = (Sexp.of_string code) in
  print_endline (eval { operations = default_ops_map } "" sexp)
  

