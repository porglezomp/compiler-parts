(*
int i, j, k;

for (i = 0; i < N; ++i)
{
    for (j = 0; j < N; ++j)
    {
        C[i][j] = 0;

        for (k = 0; k < N; ++k)
            C[i][j] += A[i][k] * B[k][j];
    }
}
*)

let mat_mult =
  let open Ast in
  let a, b, c, n = V 0, V 1, V 2, V 3 in
  let i, j, k = V 4, V 5, V 6 in
  let num_size = Int 4 in
  let idx2 base i j =
    let addr = Var base in
    let addr = Add(addr, Mul(Var i, Mul(num_size, Var n))) in
    let addr = Add(addr, Mul(Var j, num_size)) in
    addr
  in
  {
    name = "mul";
    params = [a; b; c; n];
    vars = [i; j; k];
    body = [
      For (i, Int 0, Var n, [
          For (j, Int 0, Var n, [
              Write (idx2 c i j, Int 0);
              For (k, Int 0, Var n, [
                  Write (
                    idx2 c i j,
                    Add(Read(idx2 c i j),
                        Mul(Read(idx2 a i k), Read(idx2 b k j))));
                ]);
            ]);
        ]);
      Return (Var c);
    ]
  }

let () =
  print_endline (Ast.string_of_def mat_mult) ;
  let prog = Ast.compile mat_mult in
  let out = open_out "target/mat-mult.dot" in
  Tac.graphviz prog |> output_string out
