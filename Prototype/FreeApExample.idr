module FreeApExample


import FreeApplicative

Author : String
Author = "Runnable"

Post : String
Post = "Some Post"

Id : String
Id = "001"

data BlogF : (id : String) -> Type where
  GetPost : id -> BlogF Post
  GetAuthor : id -> BlogF Author

-- data Blog a = Ap BlogF a
