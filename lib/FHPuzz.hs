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

showPt (V2 x y) = printf "V2 %6.2f %6.2f" x y :: String

data MyLine a = MyLine
    { _lColor :: Colour a
    , _pointA :: Pt
    , _pointB :: Pt
    }
makeLenses ''MyLine

instance Show (MyLine a) where
    show MyLine{..} = 
        printf "MyLine (%s) (%s)" (showPt _pointA) (showPt _pointB) :: String

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
( boxes, myLines ) = bimap position concat $ unzip
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
        -- | color == black && 
        --     traceShow ("corners",corners)

        ( ( p2 (x,-y), mkBox corners )
        , MyLine color <$> aa <*> bb )
        -- ( p2 (x,-y), rect 64 10 # lw none # fc color # rotate r )
      where
        r = (-angle) @@ deg
        corners = rotatedBoxCorners r
        aa = (+ V2 x (-y)) <$> corners
        bb = take 4 . drop 1 $ aa <> aa

fhpuzzMain :: IO ()
fhpuzzMain = do
    -- traverse_ print myLines
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
