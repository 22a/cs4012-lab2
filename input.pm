(
  (
    ( Assign "arg"     (Const (I 10)) )
    `Seq`
    ( Assign "scratch" (Var "arg")    )
  )
  `Seq`
  (
    ( Assign "total"   (Const (I 1))  )
    `Seq`
    (
      While ((Var "scratch") `Gt` (Const (I 1)))
      (
        (
          ( Assign "total"   ( (Var "total")   `Mul` (Var "scratch") ) )
          `Seq`
          ( Assign "scratch" ( (Var "scratch") `Sub` (Const (I 1))   ) )
        )
        `Seq`
        ( Print ( Var "scratch" ) )
      )
    )
  )
)
`Seq`
( Print ( Var "scratch" ) )
