module RecursionSchemes


data Lit = StrLit String
         | IntLit Int
         | Ident String


data Expr = Index Expr Expr
          | Call Expr (List Expr)
          | Unary String Expr
          | Binary Expr String Expr
          | Paren Expr
          | Literal Lit


data Stmt = Break
          | Continue
          | Empty
          | IfElse Expr (List Stmt) (List Stmt)
          | Return (Maybe Expr)
          | While Expr (List Stmt)
          | Expression Expr


flatten : Expr -> Expr
flatten (Index e i) = Index (flatten e) (flatten i)
flatten (Call e args) = Call (flatten e) (map flatten args)
flatten (Unary op arg) = Unary op (flatten arg)
flatten (Binary l op r) = Binary (flatten l) op (flatten r)
flatten (Paren e) = flatten e
flatten (Literal i) = Literal i
