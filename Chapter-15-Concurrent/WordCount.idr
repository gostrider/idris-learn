import ProcessLib

record WCData where
  constructor MkWCData
  wordCount : Nat
  lineCount : Nat


data WC = CountFile String | GetData String

WCType : WC -> Type
WCType (CountFile x) = ()
WCType (GetData x) = Maybe WCData


wcService : (loaded : List (String, WCData)) -> Service WCType ()
wcService loaded = ?wcService_rhs

doCount : (content : String) -> WCData
doCount content = let lcount = length (lines content)
                      wcount = length (words content)
                  in
                    MkWCData lcount wcount


procMain : Client ()
procMain = do
  Just wc <- Spawn (wcService []) | Nothing => Action (putStrLn "Spawn failed")
  Action (putStrLn "Counting test.txt")
  Request wc (CountFile "test.txt")
  Action (putStrLn "Processing")
  Just wcdata <- Request wc (GetData "test.txt") | Nothing => Action (putStrLn "File error")
  Action (putStrLn ("Words: " ++ show (wordCount wcdata)))
  Action (putStrLn ("Lines: " ++ show (lineCount wcdata)))
