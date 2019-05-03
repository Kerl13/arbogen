(** {2 Command line parameters} *)

type value =
  | Vint of int
  | Vfloat of float
  | Vstring of string

type parameter = Param of string * value

(** {2 Global options} *)

type options_record = {
  mutable interactive_mode : bool;
  mutable grammar_file: string;
  mutable verbosity: int;
  mutable random_seed: int option;
  mutable size_min: int;
  mutable size_min_set: bool;
  mutable size_max: int;
  mutable size_max_set: bool;
  mutable epsilon1: float;
  mutable epsilon1_set: bool;
  mutable epsilon1_factor: float;
  mutable epsilon1_factor_set: bool;
  mutable epsilon2: float;
  mutable epsilon2_set: bool;
  mutable epsilon2_factor: float;
  mutable epsilon2_factor_set: bool;
  mutable with_id : bool;
  mutable with_type : bool;
  mutable max_try: int;
  mutable max_try_set: bool;
  mutable ratio_rejected: float;
  mutable ratio_rejected_set: bool;
  mutable max_refine: int;
  mutable max_refine_set: bool;
  mutable output_type: int;
  mutable fileName: string;
  mutable zstart: float;
  mutable zstart_set: bool;
  mutable with_state:bool;
  mutable state_file:string;
  mutable randgen:string;
  mutable indent:bool;
  mutable print_oracle:bool
} ;;

let global_options = {
  interactive_mode = false;
  grammar_file = "";
  verbosity = 1;
  random_seed = None;
  size_min = 10;
  size_min_set = false;
  size_max = 20;
  size_max_set = false;
  epsilon1 = 0.001;
  epsilon1_set = false;
  epsilon1_factor = 0.1;
  epsilon1_factor_set = false;
  epsilon2 = 0.0001;
  epsilon2_set = false;
  epsilon2_factor = 0.1;
  epsilon2_factor_set = false;
  with_id = false;
  with_type = false;
  max_try = 100;
  max_try_set = false;
  ratio_rejected = 0.8;
  ratio_rejected_set = false;
  max_refine = 8;
  max_refine_set = false;
  output_type = 0;
  fileName = "";
  zstart = 0.0;
  zstart_set = false;
  with_state = false;
  state_file = "";
  randgen = "ocaml";
  indent=false;
  print_oracle=false;
}

exception Option_Error of string
