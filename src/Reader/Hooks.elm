module Reader.Hooks exposing (updateExec)

import Elm.Kernel.Reader


{-| -}
-- concreteModel must be Reader.Model.
updateExec : (msg -> model -> ( model, a )) -> msg -> model -> ( model, concreteModel )
updateExec =
    Elm.Kernel.Reader.updateExec
