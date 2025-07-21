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


inductive Tens : Type
| nil : Tens
| cons : Vec Tens n -> Tens

inductive Tensor : Shape dim -> Type
| item : Nat -> Tensor .nil
| empty : (shp:Shape n) -> Tensor (.cons 0 shp)
| stacked : Tensor shp -> Tensor (.cons n shp) -> Tensor (.cons (n+1) shp)



def NestedList : Nat -> Type
| 0 => Nat
| n+1 => List $ NestedList n




def h : 1 == 1 := rfl


def Tensor.append : (X:Tensor shp) ->

def Tensor.fromList : (n:Nat) -> NestedList n -> (shape: Shape n) × Tensor shape
| 0, l => .mk Vec.nil $ Tensor.item l

| n+1, l => match (l : List $ NestedList n) with
  | [] =>
    let p : NestedList (n+1) := []
    let shp : Shape $ n + 1 := .zeros $ n +
    sorry

  | x::xs => sorry





def x : Nat := 0

def f : (x:Nat) -> Vec Nat x
| 0 => Vec.nil
| n+1 => Vec.cons 1 (f n)
