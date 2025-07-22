def List.pop : (axis:Nat) -> (shp:List Nat) -> (List Nat)
| _ , [] => []
| 0, _::xs => xs
| axis+1, x::xs => x :: List.pop axis xs


inductive ZOp : Type
| rand
| full : (x:Nat) -> ZOp
| const : (raw: List Nat) -> ZOp

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
def Tensor.fromList (list:List Nat) := Tensor.base (ZOp.const list) [list.length]
def Tensor.add (x:Tensor shp) (y:Tensor shp) := Tensor.binary .add x y



def l : Tensor [3] := Tensor.fromList [1,2,3]
def x := Tensor.ones [3,2]
def y := x.sum 0


def BinOp.compile : BinOp -> String | .add => "add" | .sub => "sub" | .mul => "mul" | .div => "div"
def ReduceOp.compile : ReduceOp -> String | .sum => "sum" | .prod => "prod" | .mean => "mean" | .max => "max" | .min => "min"

def Tensor.compile : Tensor shp -> String

| Tensor.base op shp => match op with
  | ZOp.full x => "Tensor.full(" ++ toString shp ++ ", " ++ toString x ++ ")"
  | ZOp.rand => "Tensor.rand(" ++ shp.toString ++ ")"
  | ZOp.const raw => "Tensor(" ++ raw.toString ++ ")"

| Tensor.unary op x => match op with
  | UOp.inv => "Tensor.inv(" ++ x.compile ++ ")"
  | UOp.exp => "Tensor.exp(" ++ x.compile ++ ")"

| Tensor.binary op x y => "Tensor." ++ op.compile ++ "(" ++ x.compile ++ ", " ++ y.compile ++ ")"
| Tensor.reduce op axis x => "Tensor." ++ op.compile ++ "(" ++ toString axis ++ ", " ++ x.compile ++ ")"
| Tensor.reshape view x => match view with
  | View.permute axes => "Tensor.permute(" ++ axes.toString ++ ", " ++ x.compile ++ ")"
