module Reader.TraceData.Value
    exposing
        ( Value
        , decode
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


toString : Value -> String
toString (Value x) = Debug.toString (Elm.Kernel.Json.unwrap x)

decode : JD.Decoder Value
decode = JD.map Value JD.value
