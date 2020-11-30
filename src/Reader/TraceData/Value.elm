module Reader.TraceData.Value
    exposing
        ( Value
        , decode
        , isCtrBinop
        , isCtrTuple
        , toString
        )

import Debug
import Elm.Kernel.Reader
import Json.Decode as JD
import Json.Encode as JE
import Reader.Dict as Dict exposing (Dict)


{-| Value represents an arbitrary Elm value, from a trace.
-}
type Value = Value JD.Value


isCtrBinop : String -> Bool
isCtrBinop name =
    -- If it doesn't contain any letters, it's a binop.
    -- TODO: this can be replaced with something precise by looking through the compiler
    String.toLower name == String.toUpper name


isCtrTuple : String -> Bool
isCtrTuple name =
    String.startsWith "#" name


toString : Value -> String
toString (Value x) = "<value>" -- Debug.toString x -- FIXME to be "<value>"

decode : JD.Decoder Value
decode = JD.map Value JD.value
