module FreeApExample


import FreeApplicative

Author : String

Post : String

Id : String

data BlogF a where
  GetPost : Id -> BlogF Post
  GetAuthor : Id -> BlogF Author
