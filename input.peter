Seq
(
  Seq
  (
    Seq
    ( Assign "arg"     (Const (I 10)) )
    ( Assign "scratch" (Var "arg")    )
  )
  (
    Seq
    ( Assign "total"   (Const (I 1))  )
    (
      While (Gt (Var "scratch") (Const (I 1)))
      (
        Seq
        (
          Seq
          ( Assign "total"   ( Mul (Var "total")    (Var "scratch") ) )
          ( Assign "scratch" ( Sub (Var "scratch")  (Const (I 1))   ) )
        )
        ( Print ( Var "scratch" ) )
      )
    )
  )
)
( Print ( Var "total" ) )
