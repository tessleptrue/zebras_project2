(* C- interpreter.
 *
 * N. Danner
 *)

module Ast = Challenge_ast

(* Raised when a function body terminates without executing `return`.
 *)
exception NoReturn of Ast.Id.t

(* MultipleDeclaration x is raised when x is declared more than once in a
 * block.
 *)
exception MultipleDeclaration of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
 *)
exception UnboundVariable of Ast.Id.t

(* UndefinedFunction f is raised when f is called but has not been defined.
 *)
exception UndefinedFunction of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
 * of the incorrect type.  s is any (hopefuly useful) message.
 *)
exception TypeError of string

(* OutOfMemoryError is raised when the an attempt is made to allocate more
 * space in the store than is available.
 *)
exception OutOfMemoryError

(* Raised when an attempt is made to access a store location that is
 * negative or larger than the store capacity.
 *)
exception SegmentationError of int


(* Values.
 *)
module Value = struct
  type t = 
    | V_Undefined
    | V_None
    | V_Int of int
    | V_Bool of bool
    | V_Str of string
    | V_Loc of int
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Undefined -> "?"
    | V_None -> "None"
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
    | V_Str s -> s
    | V_Loc l -> Int.to_string l 
end

(* Module for input/output built-in functions.
 *
 * Alas, this module needs to be defined as part of the Interp module, because
 * the I/O functions rely on Value.t.  I guess the right way to do this is for
 * [fprintf] to take a list of metalanguage type values, and for [fscanf] to
 * return a disjoint sum of metalanguage type values, and let the caller
 * unpack/pack from/to [Value.t] values.
 *)
module Io = struct

  (* The input source and output destination is abstracted, because there
   * are two use cases that are rather different.  The interactive
   * interpreter uses standard input and standard output for input and
   * output.  But the automated tests need the input source and output
   * destination to be programmatic values (the former is read from a test
   * specification, and the latter has to be compared to the test
   * specification).  The "right" way to do this is to make the interpreter
   * itself a functor that takes an IO module as an argument, but that is a
   * little much for this project, so instead we define this Io module with
   * the input source (`in_channel`) and output destination (`output`)
   * references that can be changed by the client that is using the
   * interpreter.
   *)

  (* The input channel.  get_* and prompt_* read from this channel.  Default
   * is standard input.
   *)
  let in_channel : Scanf.Scanning.in_channel ref =
    ref Scanf.Scanning.stdin

  (* The output function.  printf calls this function for output.  Default is
   * to print the string to standard output and flush.
   *)
  let output : (string -> unit) ref = 
    ref (
      fun s ->
        Out_channel.output_string Out_channel.stdout s ;
        Out_channel.flush Out_channel.stdout
    )

  (* tail s = s[1..].
   *)
  let tail (s : string) : string =
    String.sub s 1 (String.length s - 1)

  (* tailtail s = tail(tail s).
   *)
  let tailtail (s : string) : string =
    tail (tail s)

  (* scons c s = String.make 1 c ^ s.
   *)
  let scons (c : char) (s : string) : string =
    String.make 1 c ^ s

  (* do_fprintf fmt vs:  print [vs] to stdout according to [fmt].
   *)
  let do_fprintf (fmt : string) (vs : Value.t list) : unit =
    let rec build_result (fmt : string) (vs : Value.t list) : string =
      if fmt = "" 
      then
        match vs with
        | [] -> ""
        | _ -> raise @@ TypeError "Too many values to print for format string"
      else if fmt.[0] != '%' then scons fmt.[0] (build_result (tail fmt) vs)
      else if String.length fmt = 1
      then raise @@ TypeError "Malformed format string (incomplete %)"
      else
        match (String.sub fmt 0 2, vs) with
        | ("%d", V_Int n :: vs) -> 
          Printf.sprintf
            "%d%s"
            n
            (build_result (tailtail fmt) vs)
        | ("%b", V_Bool b :: vs) -> 
          Printf.sprintf
            "%b%s"
            b
            (build_result (tailtail fmt) vs)
        | ("%s", V_Str s :: vs) -> 
          Printf.sprintf
            "%s%s"
            s
            (build_result (tailtail fmt) vs)
        | _ ->
          raise @@ TypeError "Bad % specifier or incorrect value type"
    in

    !output (build_result fmt vs)

  (* do_fscanf fmt = v, where v is the value read from stdin according to fmt.
   *)
  let do_fscanf (fmt : string) : Value.t =
    let fmt' : string = String.trim fmt in
    match fmt' with
    | "%d" -> Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
    | "%b" -> Value.V_Bool (Scanf.bscanf !in_channel " %b" (fun b -> b))
    | "%s" -> Value.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
    | _ ->
      raise @@ TypeError (
        Printf.sprintf
        "Bad scanf format string: %s"
        fmt
      )

end



(* Module for environments.
 *)
module Env = struct

  (* Define type var(s), funk(s) so environment contains functions and variables seperately *)
  type var = (Ast.Id.t * Value.t) 
  type vars = var list
  (* Function name, arguments, expression *)

  (*  empty = ρ, where dom ρ = ∅.
   *)
  type t = vars
  let empty : t = []
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅. lookup ρ x = ρ(x). *)
  
  let lookup (rho : t) (x : Ast.Id.t) : Value.t option =
    List.assoc_opt x rho

  (*  update ρ x v = ρ{x → v}.
   *)
  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x, v) :: List.remove_assoc x rho

  let def_var (rho : t)(x: Ast.Id.t) (v : Value.t) : t =
    match lookup rho x with
      |None -> (x, v) :: rho
      |Some _ -> raise (MultipleDeclaration x)
    

end

module Env_block = struct 
  type t = Env.t list

  let eb_add_empty (block : t) : t =
    Env.empty :: block

  let rec eb_lookup (block : t) (x: Ast.Id.t) : Value.t option = 
    match block with
      |[] -> None
      |y::ys -> match Env.lookup y x with
        |None ->  eb_lookup ys x
        |Some v -> Some v


  let rec eb_update (block : t)(x : Ast.Id.t)(v : Value.t) : t =
    match block with
    |[] -> raise (UnboundVariable x)
    |y::ys -> match Env.lookup y x with 
      |None -> y :: eb_update ys x v
      |Some _ -> (Env.update y x v) :: ys

  let eb_pop (block : t) : t = 
    match block with
    |[] -> [] (*raise fail?*)
    |_::ys -> ys

  let def_var (block : t)(x: Ast.Id.t) (v : Value.t) : t =
    match block with 
    |[] -> []
    |y::ys -> (Env.def_var y x v) :: ys

end

module Frame = struct
  type t = 
  |E_frame of Env_block.t
  |V_frame of Value.t

  let fr_empty = E_frame [Env.empty]
end


module Store = struct 

  type t = Value.t Array.t * int ref

  
  let store_make (size : int) : t =
    (Array.make size Value.V_Undefined, ref 0)

  let store_lookup (store : t) (location : int) : Value.t =
    let (arr, _) = store in
    if location < 0 || location >= Array.length arr then
      raise (SegmentationError location)
    else
      Array.get arr location

  let store_update (store : t) (location : int) (v : Value.t) : unit =
    let (arr, _) = store in
    if location < 0 || location >= Array.length arr then
      raise (SegmentationError location)
    else
      Array.set arr location v

  let store_new_loc(store : t) : int =
    let (arr, next) = store in
    let loc = !next in
    if loc >= Array.length arr then
      raise OutOfMemoryError
    else
      let _ = Array.set arr loc Value.V_Undefined in
      let _ = next := loc + 1 in
      loc

end 

let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op, v) with
  |(Ast.Expr.Not, Value.V_Bool n) -> Value.V_Bool (not n)
  |(Ast.Expr.Neg, Value.V_Int n) -> Value.V_Int (-n)
  |_  -> raise (TypeError "Invalid operand for unary operator")

let binop (op : Ast.Expr.binop) (v : Value.t) (v' : Value.t) : Value.t =
  match (op, v, v') with
  | (Ast.Expr.Plus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n + n')
  | (Ast.Expr.Minus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n - n')
  | (Ast.Expr.Times, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n * n')
  | (Ast.Expr.Div, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n / n')
  | (Ast.Expr.Mod, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n mod n')
  | (Ast.Expr.And, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n && n')
  | (Ast.Expr.Or, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n || n')
  | (Ast.Expr.Eq, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n = n')
  | (Ast.Expr.Eq, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b = b')
  | (Ast.Expr.Ne, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b != b')
  | (Ast.Expr.Ne, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n != n')
  | (Ast.Expr.Lt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n < n')
  | (Ast.Expr.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n > n')
  | (Ast.Expr.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')
  | (Ast.Expr.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')
  |_ -> raise (TypeError "Unsupported expression")


let rec def_funks (funks : Ast.Prog.fundef list) : (Ast.Id.t * (Ast.Id.t list * Ast.Stm.t list)) list = 
    match funks with
          |[] -> []
          |("main", params, body)::_ -> [("main", (params, body))]
          |(name, params, body)::xs -> (name, (params, body)) :: def_funks xs 

let rec arg_match (fr : Frame.t)(params : Ast.Id.t list) (vals : Value.t list) : Frame.t = 
    match fr with
      | Frame.V_frame v -> Frame.V_frame v (* is this the right behavior? *)
      | Frame.E_frame envs ->
        (match (params, vals) with
          | ([], []) -> fr
          | ([], _) -> raise (TypeError "too many args")
          | (_, []) -> raise (TypeError "too few args")
          | (y::ys, b::bs) -> (arg_match (Frame.E_frame(Env_block.def_var envs y b)) ys bs ))
(* exec p:  Execute the program `p`.
 *)
let exec (_ : Ast.Prog.t) : unit =
  match p with
  | Pgm fundefs -> 
    let f_list = def_funks fundefs in
  let rec eval (fr: Frame.t) (e : Ast.Expr.t) : Value.t =
    (match fr with
    |Frame.V_frame _ -> failwith "unimplemented"
    |Frame.E_frame envs -> 
      (match e with
      | Ast.Expr.Var x ->
        (match x with
        | "stdout" | "stdin" -> Value.V_None
        | _ ->
            match (Env_block.eb_lookup envs x) with
            | Some Value.V_Undefined -> raise (UnboundVariable x)
            | Some v -> v
            | None -> raise (UnboundVariable x))
      (* |Ast.Expr.Var x -> (match (Env_block.eb_lookup envs x) with 
                            | Some v -> v
                            | None -> raise (UnboundVariable x) ) *)
      | Ast.Expr.Num n -> Value.V_Int n
      | Ast.Expr.Bool b -> Value.V_Bool b
      | Ast.Expr.Unop (op, e) ->
        let v = eval fr e in 
        unop op v 
      | Ast.Expr.Binop (op, e, e') ->
        let v = eval fr e in
        let v' = eval fr e' in
        binop op v v'
      | Ast.Expr.Call (f, args) -> 
              let evaled_args = List.map (eval fr) args in
                (match f with
                | "fprintf" ->
                      (match evaled_args with
                          | _ :: Value.V_Str fmt :: rest -> 
                              let () = Io.do_fprintf fmt rest in
                              Value.V_None
                          | _ -> raise (TypeError "fprintf: bad arguments"))
                |"main" -> let (params, body) = 
                    match List.assoc_opt f f_list with 
                    | Some v -> v
                    | None -> raise (UndefinedFunction f)
                  in
                    (match eval_stms (arg_match Frame.fr_empty params evaled_args) body with
                      | Frame.V_frame v -> v
                      | Frame.E_frame _ -> Value.V_None)
                | _ ->
                  let (params, body) = 
                    match List.assoc_opt f f_list with 
                    | Some v -> v
                    | None -> raise (UndefinedFunction f)
                  in
                    (match eval_stms (arg_match Frame.fr_empty params evaled_args) body with
                      | Frame.V_frame v -> v
                      | Frame.E_frame _ -> raise (NoReturn f) ))
            |Ast.Expr.Str s -> Value.V_Str s))
          and eval_stm (fr : Frame.t) (stm : Ast.Stm.t) : Frame.t =
            (match fr with 
            |Frame.V_frame _ -> raise (TypeError "fr")
            |Frame.E_frame envs -> (match stm with
              | VarDec (xs) -> 
                let rec dec_list (fr' : Frame.t) (xs : (Ast.Id.t * Ast.Expr.t option) list) : Frame.t = 
                  match (fr', xs) with 
                    | (_, []) -> fr'
                    | (Frame.V_frame _, _) -> fr'
                    | (Frame.E_frame envs', (name, e_opt)::ys) ->
                        (match e_opt with 
                          | None -> dec_list (Frame.E_frame(Env_block.def_var envs' name Value.V_Undefined)) ys 
                          | Some e -> dec_list (Frame.E_frame(Env_block.def_var envs' name (eval fr' e))) ys)
                in
                  dec_list fr xs

              | Fscanf (_, st, x) -> Frame.E_frame(Env_block.eb_update envs x (Io.do_fscanf st))
              | Assign (x, e) -> Frame.E_frame(Env_block.eb_update envs x (eval fr e))
              | Expr e -> let _ = eval fr e in Frame.E_frame envs (* calls eval in case there are prints etc *)
              | Block stms -> (match (eval_stms (Frame.E_frame(Env_block.eb_add_empty envs)) stms) with
                              |Frame.V_frame v -> Frame.V_frame(v) 
                              |Frame.E_frame es -> Frame.E_frame(Env_block.eb_pop es))
              | IfElse (e, stm1, stm2) -> (match (eval fr e) with 
                                          |Value.V_Bool x -> (match x with 
                                                    |false -> eval_stm fr stm2 
                                                    |true -> eval_stm fr stm1 )
                                          |_-> raise (TypeError "not bool") )
              | While (e, stm) -> (match (eval fr e) with 
                                      | Value.V_Bool true -> 
                                          (match eval_stm fr stm with
                                          | Frame.V_frame v -> Frame.V_frame v
                                          | Frame.E_frame envs' -> eval_stm (Frame.E_frame envs') (While (e, stm)))
                                      | Value.V_Bool false -> fr
                                      | _ -> raise (TypeError "not bool"))
              | Return (e_opt) -> (match e_opt with
                                    |None -> Frame.V_frame(Value.V_None)
                                    |Some e -> Frame.V_frame(eval fr e)

      ))) 
          and 
          eval_stms (fr: Frame.t) (stms : Ast.Stm.t list) : Frame.t =
            (match stms with
            |[] -> fr
            |y::ys -> (match eval_stm fr y with
                      |Frame.V_frame(v) -> Frame.V_frame(v)
                      |Frame.E_frame(envs) -> eval_stms (Frame.E_frame(envs)) ys))
      in

  let _ = eval Frame.fr_empty (Call("main", [])) in
  ()




