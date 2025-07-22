
def hello : IO Unit := do
  IO.println "hello"

def runPython (code : String) : IO Unit := do

  IO.println "RUNNING PYTHON"
  let file := "out.py"
  let handle ← IO.FS.Handle.mk file IO.FS.Mode.write
  handle.putStrLn s!"from tinygrad import Tensor\n{code}"
  handle.flush
  let out ← IO.Process.output {
    cmd := "python3",
    args := #[file  ]
  }
  IO.println "out.stdout"
  IO.print out.stdout
  IO.eprint out.stderr
