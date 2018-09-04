(*
fn fib(n: int) -> int {
  let a = 0;
  let b = 1;
  while (n > 0) {
    let c = b;
    b = a + b;
    a = c;
    n = n - 1;
  }
  return a;
}
*)

let add_assign var op =
  Tac.add_instr (Assign (var, op))

let fib =
  let open Expr in
  let n, a, b, c = V 0, V 1, V 2, V 3 in
  {
    name = "fib";
    params = [n];
    body = [
      Assign (a, Int 0);
      Assign (b, Int 1);
      While (Le (Int 0, Var n), [
          Assign (c, Var b);
          Assign (b, Add (Var a, Var b));
          Assign (a, Var c);
          Assign (n, Sub (Var n, Int 1));
        ]);
      Return (Var a);
    ];
  }

let collatz =
  let open Expr in
  let max, x, n, steps = V 0, V 1, V 2, V 3 in
  {
    name = "collatz";
    params = [max];
    body = [
      Assign (n, Int 2);
      Assign (steps, Int 0);
      While (Lt (Var n, Var max), [
          Assign (x, Var n);
          Assign (n, Add(Var n, Int 1));
          While (Not (Eq (Var x, Int 1)), [
              If (Eq (Mul (Div (Var x, Int 2), Int 2), Var x),
                  [ (* even *)
                    Assign (x, Div (Var x, Int 2));
                  ],
                  [
                    Assign (x, Add (Mul (Var x, Int 3), Int 1));
                  ]
                 );
              Assign (steps, Add (Var steps, Int 1));
            ]);
        ]);
      Return (Int 0);
    ]
  }

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
  n := n - 1
  goto fib1

fib3:
  return a
*)

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
    |> add_assign c (Int 1)
    |> add_assign n (Sub (n, c))
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
  print_endline (Expr.string_of_def fib) ;
  print_newline () ;
  print_endline (Expr.string_of_def collatz) ;
  let out = open_out "target/tac.dot" in
  output_string out (Tac.graphviz prog)
