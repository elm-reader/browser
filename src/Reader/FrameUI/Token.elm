module Reader.FrameUI.Token
    exposing
        ( Token(..)
        )

import Reader.SourceMap as SourceMap exposing (SourceMap)

type Token
    = ExprStart SourceMap.ExprId -- '<span title="[expr value]">'
    | ExprEnd SourceMap.ExprId -- '</span>'
    | Character Char -- 'x'
