import TinyLean.Tensor
import TinyLean.Runtime


def RunTensor (tensor:Tensor shp) : IO Unit := do
  runPython $ "print(" ++ tensor.compile ++ ".tolist())"
