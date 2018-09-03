(*
def fib
  params:
  0 (n)
fib0:
  a := 0
  b := 1
  goto fib1

fib1:
  c := 0
  c := c <= n
  goto if c fib2 fib3

fib2:
  c := b
  b := a + b
  a := c
  goto fib1

fib3:
  return a
*)

let add_assign var op =
  Tac.add_instr (Assign (var, op))

let prog =
  let open Tac in
  let def = new_def () in
  let n, a, b, c = (new_var def, new_var def, new_var def, new_var def) in
  let fib0 = new_block def in
  let fib1 = new_block def in
  let fib2 = new_block def in
  let fib3 = new_block def in
  let fib0 =
    fib0
    |> add_assign a (Int 0)
    |> add_assign b (Int 1)
    |> set_succ (Goto (block_id fib1))
  in
  let fib1 =
    fib1
    |> add_assign c (Int 0)
    |> add_assign c (Le (c, n))
    |> set_succ (GotoIf (c, block_id fib2, block_id fib3))
  in
  let fib2 =
    fib2
    |> add_assign c (Var b)
    |> add_assign b (Add (a, b))
    |> add_assign a (Var c)
    |> set_succ (Goto (block_id fib1))
  in
  let fib3 =
    fib3
    |> set_succ (Return a)
  in
  def
  |> add_param n
  |> set_entry fib0
  |> add_block fib0
  |> add_block fib1
  |> add_block fib2
  |> add_block fib3

let () =
  let out = open_out "target/tac.dot" in
  output_string out (Tac.graphviz prog)
