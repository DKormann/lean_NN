

def List.pop : (axis:Nat) -> (shp:List Nat) -> (List Nat)
| _ , [] => []
| 0, _::xs => xs
| axis+1, x::xs => x :: List.pop axis xs


inductive ZOp : Type
| rand
| full : (x:Nat) -> ZOp

inductive UOp : Type
| inv | exp

inductive BinOp : Type
| add | sub | mul | div

inductive ReduceOp : Type
| sum | prod | mean | max | min

inductive View : Type
| permute : (axes: List Nat) -> View

def View.apply (view:View) (shp:List Nat) : List Nat :=
match view with
| .permute axes => axes.map fun x => shp[x]!

inductive Tensor : (shp: List Nat) -> Type
  | base : (op:ZOp) -> (shp: List Nat) -> Tensor shp
  | unary : (op:UOp) -> (x:Tensor shp) -> Tensor shp
  | binary : (op:BinOp) -> (x:Tensor shp) -> (y:Tensor shp) -> Tensor shp
  | reduce : (op:ReduceOp) -> (axis:Nat) -> (x:Tensor shp) -> Tensor (shp.pop axis)
  | reshape : (view : View) -> (x:Tensor shp) -> Tensor (view.apply shp)

def Tensor.zeros (shp:List Nat) := Tensor.base (ZOp.full 0) shp
def Tensor.ones (shp:List Nat) := Tensor.base (ZOp.full 1) shp
def Tensor.sum (axis:Nat) (x:Tensor shp) := Tensor.reduce .sum axis x

def x := Tensor.ones [3,2]

def y : Tensor [3] := Tensor.sum 0 x
