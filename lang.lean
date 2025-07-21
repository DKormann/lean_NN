inductive Vec (A:Type): Nat -> Type
| nil : Vec A 0
| cons: A -> Vec A n -> Vec A (n+1)


def Vec.append {A : Type} {m n : Nat} : Vec A m → Vec A n → Vec A (m + n)
| .nil, ys => by
  rw [Nat.zero_add]
  exact ys
| .cons x xs, ys => by
  rw [Nat.succ_add]
  exact .cons x (append xs ys)

instance {A : Type} {m n : Nat} : HAppend (Vec A m) (Vec A n) (Vec A (m + n)) where
  hAppend := Vec.append

def Vec.toList: Vec A n -> List A
| nil => []
| cons x xs => x :: Vec.toList xs

def Vec.fromList {A}: List A -> (n:Nat) × Vec A n
| [] => .mk 0 nil
| x :: xs =>
  let rest := Vec.fromList xs
  .mk (rest.fst + 1) $ .cons x rest.snd


def Vec.zeros (n) : Vec Nat n := match n with
| 0 => .nil
| n+1=> .cons 0 $ .zeros n


instance[ToString A] : ToString (Vec A n)  where
  toString := fun xs => "<Vec "++ toString (xs.toList) ++ " >"

#eval (Vec.fromList [1,2,3]).snd

def Shape := Vec Nat


def Shape.pop : (i:Nat) -> (shp:Shape (n+1)) -> (Shape (n))
| 0, .cons _ xs => xs
| p+1, .cons x xs => match xs with
  | .nil => .nil
  | .cons y ys => .cons x (Shape.pop p (.cons y ys))


inductive Tensor : (shp:Shape n) -> Type
| ones : (shp:Shape n) -> Tensor shp
| sum : (axis:Nat) -> Tensor shp -> Tensor (shp.pop axis)


def x := Tensor.ones $ .cons 10 $ .cons 10 .nil

def q : Tensor (.cons 10) := x.sum 0
