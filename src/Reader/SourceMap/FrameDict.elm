module Reader.SourceMap.FrameDict
    exposing
        ( FrameDict
        , empty
        , get
        , set
        , update
        , member
        )

import Dict exposing (Dict)

import Reader.SourceMap.Ids exposing (FrameId, ModuleId, PackageId(..))


type FrameDict v =
    FrameDict (Dict Key v)


empty : FrameDict v
empty = FrameDict Dict.empty


get : FrameId -> FrameDict v -> Maybe v
get id (FrameDict dict) =
    Dict.get (toKey id) dict


update : FrameId -> (v -> v) -> FrameDict v -> FrameDict v
update id updater (FrameDict dict) =
    FrameDict (Dict.update (toKey id) (Maybe.map updater) dict)

set : FrameId -> v -> FrameDict v -> FrameDict v
set id newVal (FrameDict dict) =
    FrameDict (Dict.update (toKey id) (\_ -> Just newVal) dict)


member : FrameId -> FrameDict v -> Bool
member id (FrameDict dict) =
    Dict.member (toKey id) dict


-- KEY TYPE

type alias Key =
    ( ( String -- package name
      , String -- module name
      )
    , String -- definition name
    , Int -- frame index (disambiguating frames in the same definition)
    )


toKey : FrameId -> Key
toKey { mod, def, frameIndex } =
    let
        (PackageId packageName) = mod.package
        moduleName = mod.mod
    in
    ( ( packageName, moduleName ), def, frameIndex )


fromKey : Key -> FrameId
fromKey ( ( packageName, moduleName ), defName, frameIndex ) =
    { mod =
        { package = PackageId packageName
        , mod = moduleName
        }
    , def = defName
    , frameIndex = frameIndex
    }
