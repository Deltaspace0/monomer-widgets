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
    , getPieceImagePath
    ) where

import Control.Lens
import Data.Text (Text)

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

getPieceImagePath :: Piece -> Text
getPieceImagePath BR = "assets/chess-pieces/bR.png"
getPieceImagePath BN = "assets/chess-pieces/bN.png"
getPieceImagePath BB = "assets/chess-pieces/bB.png"
getPieceImagePath BQ = "assets/chess-pieces/bQ.png"
getPieceImagePath BK = "assets/chess-pieces/bK.png"
getPieceImagePath BP = "assets/chess-pieces/bP.png"
getPieceImagePath WR = "assets/chess-pieces/wR.png"
getPieceImagePath WN = "assets/chess-pieces/wN.png"
getPieceImagePath WB = "assets/chess-pieces/wB.png"
getPieceImagePath WQ = "assets/chess-pieces/wQ.png"
getPieceImagePath WK = "assets/chess-pieces/wK.png"
getPieceImagePath WP = "assets/chess-pieces/wP.png"
