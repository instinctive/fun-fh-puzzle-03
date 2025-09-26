{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module FHPuzz where

import Prelude hiding (rotate)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

boxes :: Diagram B
boxes = position
    [ mk green  611 82   0
    , mk green  798 421  30
    , mk purple 604 220  (-30)
    , mk purple 495 551  60
    , mk purple 825 667  52
    , mk blue   273 397  (-69)
    , mk blue   234 606  (-69)
    , mk blue   439 539  (-67)
    , mk blue   449 791  20
    , mk yellow 637 291  (-39)
    , mk yellow 562 866  20
    , mk white  932 331  61
    , mk white  783 548  61
    , mk orange 777 692  0
    , mk orange 585 1035 0
    , mk orange 893 911  30
    ]
  where
    mk color x y angle =
        ( p2 (x,-y)
        , rect 64 10 # lw none # fc color # rotate r )
      where
        r = (-angle) @@ deg

fhpuzzMain :: IO ()
fhpuzzMain = do
    Right img <- loadImageEmb "full.png"
    mainWith
        (
            boxes # showOrigin
        <> 
            image img
                # sized (dims2D 1477 1218)
                # translateX 738
                # translateY (-609)
        )

-- bbox 1477x1218

-- rays 1224

-- 294
-- 417
-- 539
-- 662
-- 785
-- 907
