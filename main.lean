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


inductive Tensor : Shape dim -> Type
| item : Nat -> Tensor .nil
| empty : (shp:Shape n) -> Tensor (.cons 0 shp)
| stacked : Tensor shp -> Tensor (.cons n shp) -> Tensor (.cons (n+1) shp)



def Vec.prod : Vec Nat n -> Nat
| .nil => 1
| .cons x xs => x * Vec.prod xs


inductive TT : Shape dim -> Type
| cont : (shp:Shape n) -> Vec Nat (shp.prod) -> TT shp

def TT.data : (t : TT shp) -> Vec Nat (shp.prod)
| .cont _ d => d



def NestedList : Nat -> Type
| 0 => Nat
| n+1 => List $ NestedList n





theorem Vec.prod_add : (n:Nat) -> (shp:Shape n) -> Vec.prod (Vec.cons (n+1) shp) = (Vec.cons n shp).prod + shp.prod :=
by
intro n
intro shp
rw [Vec.prod]
rw [Nat.succ_mul]
rw [Vec.prod]
done


def TT.cons (x: TT shp) (X : TT $ .cons k shp): TT $ .cons (k+1) shp :=
  by
  let newshp := Vec.cons (k + 1) shp
  let dat :Vec Nat ((Vec.cons k shp).prod + shp.prod) := X.data ++ x.data

  -- Prove that the length of dat matches newshp.prod
  have len_eq : (Vec.cons k shp).prod + shp.prod = newshp.prod := by
    rw [← Vec.prod_add]

  -- Use the proof to construct the tensor
  exact TT.cont newshp dat





def TT.fromList : (n:Nat) -> NestedList n -> (shp: Shape n) × (TT shp)


  | 0, n => .mk .nil $ .cont .nil $ .cons n .nil

  | p+1, n =>

    let rec fn (l: NestedList (p+1)): List ((shp:Shape p) × (TT shp))
    := match l with
    | [] => []
    | x::xs =>
      let a := TT.fromList p x
      let b := (fn xs)
      a::b


    sorry

  -- | p+1, x::xs =>

  -- match p with
  -- | 0 => sorry


  -- let a := TT.fromList p x
  -- let b := TT.fromList (p+1) xs

  -- sorry





  -- let loop

  -- | p+1, l =>

  -- let loop

  -- match l with
  --   | [] => sorry
  --   | x::xs =>




  --   let b := TT.fromList (p) x

  --   sorry

  -- -- let loop : (shp:Shape p) -> NestedList p -> (shp: Shape n) × (TT shp)



  -- sorry




def x : Nat := 0

def f : (x:Nat) -> Vec Nat x
| 0 => Vec.nil
| n+1 => Vec.cons 1 (f n)
