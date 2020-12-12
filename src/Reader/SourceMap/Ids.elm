module Reader.SourceMap.Ids
    exposing
        ( FrameId
        , ModuleId
        , PackageId(..)
        , ExprId(..)
        , Name
        , moduleIdToString
        , decodeModuleId
        , frameIdToString
        , decodeFrameId
        , exprIdToInt
        , decodeExprId
        , packageIdToString
        , decodePackageId
        )

import Json.Decode as JD

-- MODULE ID


type alias ModuleId =
    { package : PackageId
    , mod : Name
    }


moduleIdToString : ModuleId -> String
moduleIdToString { package, mod } =
    packageIdToString package ++ "." ++ mod


decodeModuleId : JD.Decoder ModuleId
decodeModuleId =
    JD.map2 ModuleId
        (JD.field "package" decodePackageId)
        (JD.field "module" JD.string)


type alias Name =
    String


-- FRAME ID


type alias FrameId =
    { mod : ModuleId
    , def : Name
    , frameIndex : Int
    }


decodeFrameId : JD.Decoder FrameId
decodeFrameId =
    JD.map3 FrameId
        (JD.field "module" decodeModuleId)
        (JD.field "def" JD.string)
        (JD.field "frame_index" JD.int)


frameIdToString : FrameId -> String
frameIdToString { mod, def, frameIndex } =
    moduleIdToString mod ++ "." ++ def ++ " (subframe #" ++ String.fromInt frameIndex ++ ")"


-- EXPR ID

type ExprId
    = ExprId Int

decodeExprId : JD.Decoder ExprId
decodeExprId =
    JD.map ExprId JD.int


exprIdToInt : ExprId -> Int
exprIdToInt (ExprId i) =
    i



-- PACKAGE ID


type PackageId
    = PackageId String -- in format "author/project"


decodePackageId : JD.Decoder PackageId
decodePackageId =
    JD.map PackageId JD.string


packageIdToString : PackageId -> String
packageIdToString (PackageId str) =
    str


