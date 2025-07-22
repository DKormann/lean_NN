import TinyLean


def main : IO Unit := do

  RunTensor $ (Tensor.ones [2,2]).add (Tensor.ones [2,2])
