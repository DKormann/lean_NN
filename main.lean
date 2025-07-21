
inductive Vec (A:Type): Nat -> Type
| nil : Vec A 0
| cons: A -> Vec A n -> Vec A (n+1)


def Vec.toList: Vec A n -> List A
| nil => []
| cons x xs => x :: Vec.toList xs


def Vec.fromList {A}: List A -> (n:Nat) × Vec A n
| [] => .mk 0 nil
| x :: xs =>
  let rest := Vec.fromList xs
  .mk (rest.fst + 1) $ .cons x rest.snd

instance[ToString A] : ToString (Vec A n)  where
  toString := fun xs => "<Vec "++ toString (xs.toList) ++ " >"

#eval (Vec.fromList [1,2,3]).snd

def Shape := Vec Nat


inductive Tens -> Type
|

inductive Tensor : Shape dim -> Type
| nil : Tensor .nil
| unsqueeze : Tensor shp -> Tensor (.cons 1 shp)
| stacked : Tensor shp -> Tensor (.cons n shp) -> Tensor (.cons (n+1) shp)


def NestedList : Nat -> Type
| 0 => Unit -- ()
| 1 => List Nat -- [1,2,3]
| n+1 => List $ NestedList n


def fn n : NestedList n -> String
| x => "list."


#eval fn 1 [1,2,3]

def Tensor.fromList : (n:Nat) -> NestedList n -> (shape: Shape n) × Tensor shape
| 0, l => .mk Vec.nil Tensor.nil
| x, l => sorry
