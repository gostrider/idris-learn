module Printf

||| E.g. "%s = % d"
|||
||| Str (Lit " = " (Number End))
data Format = Number Format
            | Dubl Format
            | Chr Format
            | Str Format
            | Lit String Format
            | End

%name Format fmt

PrintfType : Format -> Type
PrintfType (Number  fmt) = (i   : Int)    -> PrintfType fmt
PrintfType (Dubl    fmt) = (d   : Double) -> PrintfType fmt
PrintfType (Chr     fmt) = (chr : Char)   -> PrintfType fmt
PrintfType (Str     fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End           = String


printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt)  acc = \i   => printfFmt fmt (acc ++ show i)
printfFmt (Dubl fmt)    acc = \d   => printfFmt fmt (acc ++ show d)
printfFmt (Chr fmt)     acc = \c   => printfFmt fmt (acc ++ show c)
printfFmt (Str fmt)     acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End           acc = acc


toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dubl (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars)        = Lit "%" (toFormat chars)
toFormat (c :: chars)          = case toFormat chars of
                                   (Lit lit chars') => Lit (strCons c lit) chars'
                                   fmt => Lit (strCons c "") fmt


printf : (fmt : String) -> PrintfType $ toFormat (unpack fmt) 
printf fmt = printfFmt _ ""
