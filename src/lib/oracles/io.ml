open Types

let pp_unlabelled fmt (oracle: unlabelled) =
  Format.fprintf fmt "z %h@." oracle.z;
  Array.iter (Format.fprintf fmt "%h@.") oracle.values

let float_of_string (s: string) : float =
  try float_of_string s
  with Failure _ -> invalid_arg (Format.sprintf "float_of_string \"%s\"" s)

let parse_unlabelled (s: string) : unlabelled =
  let lines = String.split_on_char '\n' s in
  match lines with
  | line0 :: values ->
    assert (line0.[0] = 'z');
    assert (line0.[1] = ' ');
    let z = float_of_string (String.sub line0 2 (String.length line0 - 2)) in
    let values =
      List.filter (fun v -> v <> "") values
      |> List.map float_of_string
      |> Array.of_list
    in
    {z; values}
  | [] -> invalid_arg "parse_unlabelled"
