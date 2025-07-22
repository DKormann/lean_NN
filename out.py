from tinygrad import Tensor
print(Tensor.add(Tensor.full([2, 2], 1), Tensor.full([2, 2], 1)).tolist())
