import Lake
open Lake DSL

package TinyLean

@[default_target]
lean_lib TinyLean

lean_exe tinylean where
  root := `TinyLean.main
