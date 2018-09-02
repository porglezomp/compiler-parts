let split_at n l =
  let rec split_at' n a b =
    if n == 0 then a, b
    else split_at' (n - 1) (List.hd b :: a) (List.tl b)
  in split_at' n [] l

let rec merge x y = match (x, y) with
    (xs, [])-> xs
  | ([], xs) -> xs
  | ((x :: xs), (y :: ys)) ->
    if x < y
    then x :: merge xs (y :: ys)
    else y :: merge (x :: xs) ys

let rec mergesort = function
    [] -> []
  | [x] -> [x]
  | xs ->
    let (left, right) = split_at (List.length xs / 2) xs in
    merge (mergesort left) (mergesort right)

type expr = Val of float
          | Add of expr * expr
          | Sub of expr * expr
          | Mul of expr * expr
          | Div of expr * expr

let rec eval = function
    Val n -> n
  | Add (a, b) -> eval a +. eval b
  | Sub (a, b) -> eval a -. eval b
  | Mul (a, b) -> eval a *. eval b
  | Div (a, b) -> eval a /. eval b
