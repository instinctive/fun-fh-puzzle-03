{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

{-# LANGUAGE TemplateHaskell           #-}

module FHPuzz where

import Prelude hiding (rotate)

import Control.Lens hiding ((#),none,transform)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Linear.V2

type Pt = V2 Double

data MyLine a = MyLine
    { _lColor :: Colour a
    , _pointA :: Pt
    , _pointB :: Pt
    } deriving Show
makeLenses ''MyLine

-- bbox 1477x1218

-- rays 1224

-- 294
-- 417
-- 539
-- 662
-- 785
-- 907

rotatedBoxCorners r =
    transform (rotation r) <$> corners
  where
    (w,h) = (64,10)
    corners =
        [ V2 (-w/2) (-h/2)
        , V2 ( w/2) (-h/2)
        , V2 ( w/2) ( h/2)
        , V2 (-w/2) ( h/2)
        ]

mkBox corners@(first@(V2 dx dy):_) =
    fromVertices [ p2 (x,y) | V2 x y <- corners <> [first] ]
    # strokeLine # lc red # lw ultraThin
    # translateX dx . translateY dy

boxes :: Diagram B
boxes = position
    [ mk  green 611   82 $   0
    , mk  green 798  421 $  30
    , mk purple 604  220 $ -30
    , mk purple 495  551 $  60
    , mk purple 825  667 $  52
    , mk   blue 273  397 $ -69
    , mk   blue 234  606 $ -69
    , mk   blue 439  539 $ -67
    , mk   blue 449  791 $  20
    , mk yellow 637  291 $ -39
    , mk yellow 562  866 $  20
    , mk  white 932  331 $  61
    , mk  white 783  548 $  61
    , mk orange 777  692 $   0
    , mk orange 585 1035 $   0
    , mk orange 893  911 $  30
    , mk black  467  331 $  39
    ]
  where
    mk color x y angle =
        let r = (-angle) @@ deg in
        ( p2 (x,-y), mkBox $ rotatedBoxCorners r )
        -- ( p2 (x,-y), rect 64 10 # lw none # fc color # rotate r )

fhpuzzMain :: IO ()
fhpuzzMain = do
    Right img <- loadImageEmb "full.png"
    mainWith
        (
            boxes
        <> 
            image img
                # sized (dims2D 1477 1218)
                # translateX 738
                # translateY (-609)
        )
