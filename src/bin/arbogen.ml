(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Arbogen                                       *
 * -------                                               *
 * Main module and Argument parser                       *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Frontend
open Options
module WeightedGrammar = Boltzmann.WeightedGrammar

let version_str = "arbogen v1.0c"
let usage = "Usage: arbogen <opt> <specfile>.spec"
let banner = "
              A      ...:'....:'':...':......
              R    :''   ._   .  `.  \\   ,   '':
              B    ':  .   \" .|    \\  `>/   _.-':
              O   .:'  .`'.   `-.  '. /'  ,..  .:
              G  :'        `.    `\\| \\./   ' :
              E  :. ,,-'''''  \"-.   |   | ....:
              N   '.      ..'''  `\\ :   |
                    ''''''''       \\'   |
           *fast* uniform random    |  =|
                  tree generator    |   |
                                    |-  |
              '''''''''''''''''''''''''''''''''''''''
              (C) F. Peschanski et al. under the GPL\n"

let speclist =
  let set_verbosity n =
    if n < 0 then begin
      Format.eprintf "Error: wrong verbosity level %d => must be positive@." n;
      exit 1
    end else global_options.verbosity <- n
  in

  [("-version", Arg.Unit (fun () -> Format.printf "%s@." version_str; exit 0),
    "print version information");

   ("-verbose", Arg.Int set_verbosity,
    "<n> : set the verbosity level to <n>  (a non-negative positive integer)");

   ("-v", Arg.Int set_verbosity,
    "<n> : same as -verbose <n>");

   ("-min", Arg.Int (fun n -> ParseUtil.set_option "min" (Vint n)),
    "<n> : set the minimum size for the generated tree to <n> (a non-negative integer)");

   ("-max", Arg.Int (fun n -> ParseUtil.set_option "max" (Vint n)),
    "<n> : set the maximum size for the generated tree to <n> (a non-negative integer)");

   ("-seed", Arg.Int (fun n -> ParseUtil.set_option "seed" (Vint n)),
    "<n> : set the random generator seed to <n>");

   ("-eps1", Arg.Float (fun f -> ParseUtil.set_option "eps1" (Vfloat f)),
    "<x> : set the epsilon for singularity search (a positive float number)");

   ("-eps2", Arg.Float (fun f -> ParseUtil.set_option "eps2" (Vfloat f)),
    "<x> : set the epsilon for simple iteration (a positive float number)");

   ("-try", Arg.Int (fun n -> ParseUtil.set_option "try" (Vint n)),
    "<n> : set the maximum of tries when generating trees");

   ("-otype", Arg.String (function
        | "arb" -> global_options.output_type <- 0;
        | "dot" -> global_options.output_type <- 1;
        | "xml" -> global_options.output_type <- 2;
        | "all" -> global_options.output_type <- 3;
        | _ ->
          Format.eprintf "Error: otype must be in [arb|dot|xml|all]@.";
          exit 1),
    "<n>: set the type [arb|dot|xml|all] of the generated tree");

   ("-o",Arg.String (fun x -> global_options.fileName <- x),
    "<x>: set the name of the file to be created at end of execution");

   ("-zstart", Arg.Float (fun f -> ParseUtil.set_option "zstart" (Vfloat f)),
    "<x>: sets the value of zstart");

   ("-state", Arg.String (fun x ->
        global_options.state_file <- x;
        global_options.with_state <- true;
      ),
    "<n>: set the name of state file");

   ("-id", Arg.Unit (fun () -> global_options.with_id <- true),
    ": number the nodes");

   ("-typ", Arg.Unit (fun () -> global_options.with_type <- true),
    ": show the type of nodes");

   ("-randgen", Arg.String (fun s -> ParseUtil.set_option "randgen" (Vstring s)),
    "[ocaml|randu|randnull] : set the random number generator");

   ("-print-oracle", Arg.String (fun s -> ParseUtil.set_option "print_oracle" (Vstring s)),
    ": output an oracle");

   ("-use-oracle", Arg.String (fun s -> ParseUtil.set_option "use_oracle" (Vstring s)),
    ": use oracle from file");

   ("-indent", Arg.Unit (fun () -> global_options.indent <- true),
    ": indent the output")
  ]

let print_tree tree =
  (* XXX. ugly workaround *)
  let tree = Tree.annotate tree in

  let {with_type; with_id; indent; _} = global_options in
  let arb_printer = Tree.output_arb ~show_type:with_type ~show_id:with_id ~indent in
  let dot_printer = Tree.output_dot ~show_type:with_type ~show_id:with_id ~indent in
  let xml_printer = Tree.output_xml ~show_type:with_type ~show_id:with_id ~indent in

  let print printer filename typ =
    if filename = "" then printer stdout tree
    else begin
      Format.printf "Saving file to '%s%s'@." filename typ;
      let out = open_out (filename ^ typ) in
      printer out tree;
      close_out out
    end
  in

  match global_options.output_type with
  | 0 -> print arb_printer global_options.fileName ".arb"
  | 1 -> print dot_printer global_options.fileName ".dot"
  | 2 -> print xml_printer global_options.fileName ".xml"
  | 3 ->
    let filename = if global_options.fileName = "" then "tree" else global_options.fileName in
    print arb_printer filename ".arb";
    print dot_printer filename ".dot";
    print xml_printer filename ".xml"
  |_ -> failwith "unreachable case"

let parse_grammar () =
  let opts, g = ParseUtil.parse_from_file global_options.grammar_file in
  ParseUtil.set_options ~preserve:true opts;
  g

let make_oracle grammar =
  if global_options.use_oracle = "" then begin
    let open Oracles.Naive in
    let oracle_config = {
        epsilon1 = global_options.epsilon1;
        epsilon2 = global_options.epsilon2;
        zmin = 0.;
        zmax = 1.;
        zstart = global_options.zstart;
      } in
    make oracle_config grammar
  end else begin
    let ic = open_in global_options.use_oracle in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    let s = Bytes.unsafe_to_string s in
    Oracles.Io.parse_unlabelled s
  end

let get_rng : string -> (module Randtools.Sig.S) = function
  | "ocaml" -> (module Randtools.OcamlRandom)
  | "randu" -> (module Randtools.Randu)
  | "randnull" -> (module Randtools.Randnull)
  | name -> Format.kasprintf invalid_arg "Unknown PRNG: %s" name

let init_rng () =
  let module Rand = (val get_rng global_options.randgen) in
  let seed = match global_options.random_seed with
    | Some seed -> seed
    | None -> Rand.self_init (); Rand.int 274537
  in
  if global_options.verbosity >= 2 then
    Format.printf "[SEED] starting seed = %d@." seed;
  Rand.init seed;
  (module Rand: Randtools.Sig.S)

let () =
  Arg.parse speclist
    (fun arg ->
       if global_options.grammar_file = ""
       then global_options.grammar_file <- arg
       else (Format.eprintf "Error: grammar file already set, argument '%s' rejected@." arg ; exit 1))
    usage;
  ParseUtil.extra_checks ();

  if global_options.verbosity > 0 then Format.printf "%s@." banner;

  if global_options.print_oracle <> "" then begin
    let filename = global_options.print_oracle in
    let fmt =
      if filename = "-" then Format.std_formatter
      else Format.formatter_of_out_channel (open_out filename)
    in
    let grammar = parse_grammar () in
    let oracle = make_oracle grammar in
    Format.fprintf fmt "%a" Oracles.Io.pp_unlabelled oracle;
    if global_options.verbosity > 0 then
      Format.printf "Oracle written to %s@." filename;
    exit 0
  end;

  let state =
    if global_options.with_state then begin
      if global_options.verbosity > 0 then
        Format.printf "Loading state file: %s@." global_options.state_file;
      let state = Boltzmann.GenState.from_file global_options.state_file in
      global_options.randgen <- state.randgen;
      Some state
    end else
      None
  in

  let module Rng = (val
    match state with
    | Some s -> get_rng s.randgen
    | None -> init_rng ()
  ) in

  let result, wgrm = match state with
    | None -> 
      let grammar = parse_grammar () in
      let oracle = make_oracle grammar in
      if (global_options.verbosity) > 0 then Format.printf "Generating tree...@.";
      let tree = Boltzmann.Gen.generator
        grammar
        oracle
        (module Rng)
        ~size_min:global_options.size_min
        ~size_max:global_options.size_max
        ~max_try:global_options.max_try
      in
      tree, WeightedGrammar.of_grammar oracle grammar
    | Some state ->
      Rng.(State.from_bytes state.rnd_state |> set_state);
      let tree = Boltzmann.Gen.free_gen (module Rng) state.weighted_grammar in
      Some tree, state.weighted_grammar
  in

  match result with
  | None ->
    Format.eprintf "No tree generated ==> try to use different parameters@.";
    exit 1
  | Some (tree, size) ->
    let final_state = Boltzmann.GenState.{
      randgen = Rng.name;
      rnd_state = Rng.(State.to_bytes (get_state ()));
      weighted_grammar = wgrm
    } in

    if global_options.verbosity > 0 then Format.eprintf "size: %d@." size;

    if global_options.with_state then begin
      let filename = global_options.state_file in
      if global_options.verbosity >= 2 then
        Format.printf "==> Saving state to file '%s'@." filename;
      Boltzmann.GenState.to_file filename final_state;
    end;

    print_tree tree
