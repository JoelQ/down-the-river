module River.Random exposing (allowedNextBlocks)

import River exposing (Block(..))


allowedNextBlocks : Block -> List Block
allowedNextBlocks block =
    case block of
        TwoTop ->
            [ TwoTop, OneTop, OneMiddle, NoBlocks ]

        TwoBottom ->
            [ TwoBottom, OneBottom, OneMiddle, NoBlocks ]

        OneTop ->
            [ TwoTop, OneTop, OneBottom, OneMiddle, TopAndBottom, NoBlocks ]

        OneBottom ->
            [ TwoBottom, OneTop, OneBottom, OneMiddle, TopAndBottom, NoBlocks ]

        OneMiddle ->
            [ OneMiddle, NoBlocks ]

        TopAndBottom ->
            [ OneTop, OneBottom, TopAndBottom, NoBlocks ]

        NoBlocks ->
            [ TwoTop, TwoBottom, OneTop, OneBottom, OneMiddle, TopAndBottom, NoBlocks ]
