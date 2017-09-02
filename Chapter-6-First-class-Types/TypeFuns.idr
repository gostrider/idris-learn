module TypeFuns


StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int


getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "runnable"
getStringOrInt True = 94


valToString : (isInt : Bool) -> (case isInt of
                                      False => String
                                      True => Int) -> String
valToString False x = trim x
valToString True x = cast x
