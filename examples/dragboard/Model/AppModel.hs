{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.Piece
    , AppModel(..)
    , boardRows
    , boardCols
    , boardState
    , initModel
    , getPathOrColor
    ) where

import Control.Lens
import Data.Text (Text)
import Monomer

import Model.Piece

data AppModel = AppModel
    { _amBoardRows :: Int
    , _amBoardCols :: Int
    , _amBoardState :: [[Piece]]
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel 8 8
    [ [BR], [BN], [BB], [BQ], [BK], [BB], [BN], [BR]
    , [BP], [BP], [BP], [BP], [BP], [BP], [BP], [BP]
    , [], [], [], [], [], [], [], []
    , [], [], [], [], [], [], [], []
    , [], [], [], [], [], [], [], []
    , [], [], [], [], [], [], [], []
    , [WP], [WP], [WP], [WP], [WP], [WP], [WP], [WP]
    , [WR], [WN], [WB], [WQ], [WK], [WB], [WN], [WR]
    ]

getPathOrColor :: Piece -> Either Text Color
getPathOrColor BR = Left "assets/chess-pieces/bR.png"
getPathOrColor BN = Left "assets/chess-pieces/bN.png"
getPathOrColor BB = Left "assets/chess-pieces/bB.png"
getPathOrColor BQ = Left "assets/chess-pieces/bQ.png"
getPathOrColor BK = Left "assets/chess-pieces/bK.png"
getPathOrColor BP = Left "assets/chess-pieces/bP.png"
getPathOrColor WR = Left "assets/chess-pieces/wR.png"
getPathOrColor WN = Left "assets/chess-pieces/wN.png"
getPathOrColor WB = Left "assets/chess-pieces/wB.png"
getPathOrColor WQ = Left "assets/chess-pieces/wQ.png"
getPathOrColor WK = Left "assets/chess-pieces/wK.png"
getPathOrColor WP = Left "assets/chess-pieces/wP.png"
