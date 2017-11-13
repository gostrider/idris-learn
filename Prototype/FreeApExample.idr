module FreeApExample


import FreeApplicative

Author : String

Post : String

Id : String

data BlogF : (id : String) -> Type where
  GetPost : id -> BlogF Post
  GetAuthor : id -> BlogF Author

data Blog a = Ap BlogF a
