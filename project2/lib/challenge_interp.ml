(* C- interpreter (Challenge: arrays + store).
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
 * of the incorrect type.  s is any (hopefully useful) message.
 *)
exception TypeError of string

(* OutOfMemoryError is raised when an attempt is made to allocate more
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
 *)
module Io = struct

  let in_channel : Scanf.Scanning.in_channel ref =
    ref Scanf.Scanning.stdin

  let output : (string -> unit) ref = 
    ref (
      fun s ->
        Out_channel.output_string Out_channel.stdout s ;
        Out_channel.flush Out_channel.stdout
    )

  let tail (s : string) : string =
    String.sub s 1 (String.length s - 1)

  let tailtail (s : string) : string =
    tail (tail s)

  let scons (c : char) (s : string) : string =
    String.make 1 c ^ s

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
        | ("%d", Value.V_Int n :: vs) -> 
          Printf.sprintf "%d%s" n (build_result (tailtail fmt) vs)
        | ("%b", Value.V_Bool b :: vs) -> 
          Printf.sprintf "%b%s" b (build_result (tailtail fmt) vs)
        | ("%s", Value.V_Str s :: vs) -> 
          Printf.sprintf "%s%s" s (build_result (tailtail fmt) vs)
        | _ ->
          raise @@ TypeError "Bad % specifier or incorrect value type"
    in
    !output (build_result fmt vs)

  let do_fscanf (fmt : string) : Value.t =
    let fmt' : string = String.trim fmt in
    match fmt' with
    | "%d" -> Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
    | "%b" -> Value.V_Bool (Scanf.bscanf !in_channel " %b" (fun b -> b))
    | "%s" -> Value.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
    | _ ->
      raise @@ TypeError (
        Printf.sprintf "Bad scanf format string: %s" fmt
      )

end

(* Module for environments.
 *)
module Env = struct

  type var = (Ast.Id.t * Value.t)
  type vars = var list

  type t = vars
  let empty : t = []

  let lookup (rho : t) (x : Ast.Id.t) : Value.t option =
    List.assoc_opt x rho

  let update (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x, v) :: List.remove_assoc x rho

  let def_var (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    match lookup rho x with
    | None -> (x, v) :: rho
    | Some _ -> raise (MultipleDeclaration x)

end

(* Module for environment blocks (non-empty lists of environments).
 *)
module Env_block = struct
  type t = Env.t list

  let eb_add_empty (block : t) : t =
    Env.empty :: block

  let rec eb_lookup (block : t) (x : Ast.Id.t) : Value.t option =
    match block with
    | [] -> None
    | y :: ys ->
      (match Env.lookup y x with
       | None -> eb_lookup ys x
       | Some v -> Some v)

  let rec eb_update (block : t) (x : Ast.Id.t) (v : Value.t) : t =
    match block with
    | [] -> raise (UnboundVariable x)
    | y :: ys ->
      (match Env.lookup y x with
       | None -> y :: eb_update ys x v
       | Some _ -> (Env.update y x v) :: ys)

  let eb_pop (block : t) : t =
    match block with
    | [] -> []
    | _ :: ys -> ys

  let def_var (block : t) (x : Ast.Id.t) (v : Value.t) : t =
    match block with
    | [] -> []
    | y :: ys -> (Env.def_var y x v) :: ys

end

(* Module for frames (either an environment block or a return value).
 *)
module Frame = struct
  type t =
    | E_frame of Env_block.t
    | V_frame of Value.t

  let fr_empty = E_frame [Env.empty]
end

(* Module for the store (a fixed-size array of values with an allocation pointer).
 *)
module Store = struct

  (* A store is a pair of a Value.t array and a mutable next-free-location pointer. *)
  type t = Value.t Array.t * int ref

  (* store_make size = a new store with the given capacity, all V_Undefined. *)
  let store_make (size : int) : t =
    (Array.make size Value.V_Undefined, ref 0)

  (* store_lookup store location = the value at location in store.
   * Raises SegmentationError if location is out of bounds.
   *)
  let store_lookup (store : t) (location : int) : Value.t =
    let (arr, _) = store in
    if location < 0 || location >= Array.length arr then
      raise (SegmentationError location)
    else
      Array.get arr location

  (* store_update store location v = unit; sets location in store to v.
   * Raises SegmentationError if location is out of bounds.
   *)
  let store_update (store : t) (location : int) (v : Value.t) : unit =
    let (arr, _) = store in
    if location < 0 || location >= Array.length arr then
      raise (SegmentationError location)
    else
      Array.set arr location v

  (* store_new_loc store = the base location for a new allocation of size 1.
   * Raises OutOfMemoryError if the store is full.
   *)
  let store_new_loc (store : t) : int =
    let (arr, next) = store in
    let loc = !next in
    if loc >= Array.length arr then
      raise OutOfMemoryError
    else begin
      Array.set arr loc Value.V_Undefined ;
      next := loc + 1 ;
      loc
    end

  (* store_alloc store n = the base location for a new allocation of size n.
   * Raises OutOfMemoryError if there is insufficient space.
   *)
  let store_alloc (store : t) (n : int) : int =
    let (arr, next) = store in
    let base = !next in
    if base + n > Array.length arr then
      raise OutOfMemoryError
    else begin
      for i = base to base + n - 1 do
        Array.set arr i Value.V_Undefined
      done ;
      next := base + n ;
      base
    end

end

(* Unary operator evaluation. *)
let unop (op : Ast.Expr.unop) (v : Value.t) : Value.t =
  match (op, v) with
  | (Ast.Expr.Not, Value.V_Bool n) -> Value.V_Bool (not n)
  | (Ast.Expr.Neg, Value.V_Int n)  -> Value.V_Int (-n)
  | _ -> raise (TypeError "Invalid operand for unary operator")

(* Binary operator evaluation. *)
let binop (op : Ast.Expr.binop) (v : Value.t) (v' : Value.t) : Value.t =
  match (op, v, v') with
  | (Ast.Expr.Plus,  Value.V_Int n,  Value.V_Int n')  -> Value.V_Int  (n + n')
  | (Ast.Expr.Minus, Value.V_Int n,  Value.V_Int n')  -> Value.V_Int  (n - n')
  | (Ast.Expr.Times, Value.V_Int n,  Value.V_Int n')  -> Value.V_Int  (n * n')
  | (Ast.Expr.Div,   Value.V_Int n,  Value.V_Int n')  -> Value.V_Int  (n / n')
  | (Ast.Expr.Mod,   Value.V_Int n,  Value.V_Int n')  -> Value.V_Int  (n mod n')
  | (Ast.Expr.And,   Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n && n')
  | (Ast.Expr.Or,    Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n || n')
  | (Ast.Expr.Eq,    Value.V_Int n,  Value.V_Int n')  -> Value.V_Bool (n = n')
  | (Ast.Expr.Eq,    Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b = b')
  | (Ast.Expr.Ne,    Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b <> b')
  | (Ast.Expr.Ne,    Value.V_Int n,  Value.V_Int n')  -> Value.V_Bool (n <> n')
  | (Ast.Expr.Lt,    Value.V_Int n,  Value.V_Int n')  -> Value.V_Bool (n < n')
  | (Ast.Expr.Gt,    Value.V_Int n,  Value.V_Int n')  -> Value.V_Bool (n > n')
  | (Ast.Expr.Le,    Value.V_Int n,  Value.V_Int n')  -> Value.V_Bool (n <= n')
  | (Ast.Expr.Ge,    Value.V_Int n,  Value.V_Int n')  -> Value.V_Bool (n >= n')
  | _ -> raise (TypeError "Unsupported binary expression")

(* Build the function definition table from the program's fundef list. *)
let rec def_funks
    (funks : Ast.Prog.fundef list)
  : (Ast.Id.t * (Ast.Id.t list * Ast.Stm.t list)) list =
  match funks with
  | [] -> []
  | ("main", params, body) :: _ -> [("main", (params, body))]
  | (name, params, body) :: xs  -> (name, (params, body)) :: def_funks xs

(* Bind parameters to argument values in a fresh frame. *)
let rec arg_match
    (fr : Frame.t)
    (params : Ast.Id.t list)
    (vals : Value.t list)
  : Frame.t =
  match fr with
  | Frame.V_frame v -> Frame.V_frame v
  | Frame.E_frame envs ->
    (match (params, vals) with
     | ([], [])      -> fr
     | ([], _)       -> raise (TypeError "too many args")
     | (_, [])       -> raise (TypeError "too few args")
     | (y :: ys, b :: bs) ->
       arg_match (Frame.E_frame (Env_block.def_var envs y b)) ys bs)

(* exec p:  Execute the program `p`.
 *)
let exec (p : Ast.Prog.t) : unit =
  match p with
  | Pgm fundefs ->

    (* Build the function table and the store (size 100 per spec). *)
    let f_list = def_funks fundefs in
    let store  = Store.store_make 100 in

    let rec eval (fr : Frame.t) (e : Ast.Expr.t) : Value.t =
      match fr with
      | Frame.V_frame _ -> failwith "eval called on a return frame"
      | Frame.E_frame envs ->
        (match e with

         (* Variables: stdout/stdin are special; otherwise look up in env block. *)
         | Ast.Expr.Var x ->
           (match x with
            | "stdout" | "stdin" -> Value.V_None
            | _ ->
              (match Env_block.eb_lookup envs x with
               | Some Value.V_Undefined -> raise (UnboundVariable x)
               | Some v -> v
               | None   -> raise (UnboundVariable x)))

         | Ast.Expr.Num  n -> Value.V_Int  n
         | Ast.Expr.Bool b -> Value.V_Bool b
         | Ast.Expr.Str  s -> Value.V_Str  s

         | Ast.Expr.Unop (op, e) ->
           unop op (eval fr e)

         | Ast.Expr.Binop (op, e, e') ->
           binop op (eval fr e) (eval fr e')

         (* Index expression: xs[e]  — look up location in store. *)
         | Ast.Expr.Index (x, e_idx) ->
           let base =
             match Env_block.eb_lookup envs x with
             | Some (Value.V_Loc l) -> l
             | Some _ -> raise (TypeError "index into non-array variable")
             | None   -> raise (UnboundVariable x)
           in
           let offset =
             match eval fr e_idx with
             | Value.V_Int n -> n
             | _ -> raise (TypeError "array index must be an integer")
           in
           Store.store_lookup store (base + offset)

         | Ast.Expr.Call (f, args) ->
           let evaled_args = List.map (eval fr) args in
           (match f with

            | "fprintf" ->
              (match evaled_args with
               | _ :: Value.V_Str fmt :: rest ->
                 Io.do_fprintf fmt rest ; Value.V_None
               | _ -> raise (TypeError "fprintf: bad arguments"))

            | "main" ->
              let (params, body) =
                match List.assoc_opt f f_list with
                | Some v -> v
                | None   -> raise (UndefinedFunction f)
              in
              (match eval_stms (arg_match Frame.fr_empty params evaled_args) body with
               | Frame.V_frame v   -> v
               | Frame.E_frame _   -> Value.V_None)

            | _ ->
              let (params, body) =
                match List.assoc_opt f f_list with
                | Some v -> v
                | None   -> raise (UndefinedFunction f)
              in
              (match eval_stms (arg_match Frame.fr_empty params evaled_args) body with
               | Frame.V_frame v -> v
               | Frame.E_frame _ -> raise (NoReturn f))))

    and eval_stm (fr : Frame.t) (stm : Ast.Stm.t) : Frame.t =
      match fr with
      | Frame.V_frame _ -> raise (TypeError "eval_stm called on a return frame")
      | Frame.E_frame envs ->
        (match stm with

         (* Scalar variable declaration (possibly with initializer). *)
         | VarDec xs ->
           let rec dec_list (fr' : Frame.t) (xs : (Ast.Id.t * Ast.Expr.t option) list) : Frame.t =
             match (fr', xs) with
             | (_, []) -> fr'
             | (Frame.V_frame _, _) -> fr'
             | (Frame.E_frame envs', (name, e_opt) :: ys) ->
               let v =
                 match e_opt with
                 | None   -> Value.V_Undefined
                 | Some e -> eval fr' e
               in
               dec_list (Frame.E_frame (Env_block.def_var envs' name v)) ys
           in
           dec_list fr xs

         (* Array variable declaration: int xs[e]; *)
         | ArrayDec xs ->
           (* xs is a list of (name, size_expr) pairs. *)
           let rec arr_dec_list
               (fr' : Frame.t)
               (xs : (Ast.Id.t * Ast.Expr.t) list)
             : Frame.t =
             match (fr', xs) with
             | (_, []) -> fr'
             | (Frame.V_frame _, _) -> fr'
             | (Frame.E_frame envs', (name, e_size) :: ys) ->
               let n =
                 match eval fr' e_size with
                 | Value.V_Int n when n >= 0 -> n
                 | Value.V_Int _ ->
                   raise (TypeError "array size must be non-negative")
                 | _ -> raise (TypeError "array size must be an integer")
               in
               let base = Store.store_alloc store n in
               arr_dec_list
                 (Frame.E_frame (Env_block.def_var envs' name (Value.V_Loc base)))
                 ys
           in
           arr_dec_list fr xs

         | Fscanf (_, st, x) ->
           Frame.E_frame (Env_block.eb_update envs x (Io.do_fscanf st))

         | Assign (x, e) ->
           Frame.E_frame (Env_block.eb_update envs x (eval fr e))

         (* Index assignment: xs[e0] = e1 *)
         | IndexAssign (x, e_idx, e_val) ->
           let base =
             match Env_block.eb_lookup envs x with
             | Some (Value.V_Loc l) -> l
             | Some _ -> raise (TypeError "index-assign into non-array variable")
             | None   -> raise (UnboundVariable x)
           in
           let offset =
             match eval fr e_idx with
             | Value.V_Int n -> n
             | _ -> raise (TypeError "array index must be an integer")
           in
           let v = eval fr e_val in
           Store.store_update store (base + offset) v ;
           Frame.E_frame envs

         | Expr e ->
           let _ = eval fr e in Frame.E_frame envs

         | Block stms ->
           (match eval_stms (Frame.E_frame (Env_block.eb_add_empty envs)) stms with
            | Frame.V_frame v -> Frame.V_frame v
            | Frame.E_frame es -> Frame.E_frame (Env_block.eb_pop es))

         | IfElse (e, stm1, stm2) ->
           (match eval fr e with
            | Value.V_Bool true  -> eval_stm fr stm1
            | Value.V_Bool false -> eval_stm fr stm2
            | _ -> raise (TypeError "if condition must be a boolean"))

         | While (e, stm) ->
           (match eval fr e with
            | Value.V_Bool false -> fr
            | Value.V_Bool true  ->
              (match eval_stm fr stm with
               | Frame.V_frame v    -> Frame.V_frame v
               | Frame.E_frame envs' ->
                 eval_stm (Frame.E_frame envs') (While (e, stm)))
            | _ -> raise (TypeError "while condition must be a boolean"))

         | Return e_opt ->
           (match e_opt with
            | None   -> Frame.V_frame Value.V_None
            | Some e -> Frame.V_frame (eval fr e)))

    and eval_stms (fr : Frame.t) (stms : Ast.Stm.t list) : Frame.t =
      match stms with
      | [] -> fr
      | y :: ys ->
        (match eval_stm fr y with
         | Frame.V_frame v    -> Frame.V_frame v
         | Frame.E_frame envs -> eval_stms (Frame.E_frame envs) ys)
    in

    let _ = eval Frame.fr_empty (Call ("main", [])) in
    ()