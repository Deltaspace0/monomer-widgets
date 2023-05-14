{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.DragboardSpec (spec) where

import Control.Lens
import Data.Text (Text)
import Monomer
import Monomer.TestEventUtil
import Monomer.TestUtil
import Test.Hspec
import qualified Data.Sequence as Seq

import Monomer.Dragboard

data Event
    = ValueChanged (Int, Int)
    | GotFocus Path
    | LostFocus Path
    deriving (Eq, Show)

data TestModel = TestModel
    { _tmField :: [[Double]]
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'TestModel

spec :: Spec
spec = describe "Dragboard" $ do
    clicking
    dragging
    validateMove
    handleEvent
    handleEventV

clicking :: Spec
clicking = describe "clicking" $ do
    let wenv = mockWenvEvtUnit $ TestModel [[42], []]
        node = dragboard 2 1 field getColor
        noClick = dragboard_ 2 1 field getColor [disableClick]
        evtC (x1, y1) (x2, y2) =
            [ evtRelease $ Point x1 y1
            , evtRelease $ Point x2 y2
            ]
        model p1 p2 n = nodeHandleEventModel wenv (evtC p1 p2) n
        f = view field
    it "should update the model when clicked from nonempty square" $
        f (model (5, 30) (400, 30) node) `shouldBe` [[], [42]]
    it "should do nothing when clicked from empty square" $
        f (model (325, 5) (5, 5) node) `shouldBe` [[42], []]
    it "should do nothing when clicked with disableClick config" $
        f (model (5, 30) (400, 30) noClick) `shouldBe` [[42], []]

dragging :: Spec
dragging = describe "dragging" $ do
    let wenv = mockWenvEvtUnit $ TestModel [[42], []]
        node = dragboard 2 1 field getColor
        model p1 p2 = nodeHandleEventModel wenv (evtDrag p1 p2) node
        f = view field
    it "should update the model when dragged from nonempty square" $
        f (model (Point 5 30) (Point 400 30)) `shouldBe` [[], [42]]
    it "should do nothing when dragged from empty square" $
        f (model (Point 325 5) (Point 5 5)) `shouldBe` [[42], []]

validateMove :: Spec
validateMove = describe "validateMove" $ do
    let wenv = mockWenvEvtUnit $ TestModel [[42], [34]]
        node = dragboard_ 2 1 field getColor [moveValidator vm]
        vm (b, ixTo, ixFrom) = (head $ b!!ixTo) <= (head $ b!!ixFrom)
        model p1 p2 = nodeHandleEventModel wenv (evtDrag p1 p2) node
        f = view field
    it "should update the model if move is valid" $
        f (model (Point 5 30) (Point 400 30)) `shouldBe` [[], [42]]
    it "should not update the model if move is not valid" $
        f (model (Point 325 5) (Point 5 5)) `shouldBe` [[42], [34]]

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
    let wenv = mockWenv $ TestModel [[42], []]
        node = dragboard_ 2 1 field getColor
            [ onChange onChangeEvent
            , onFocus GotFocus
            , onBlur LostFocus
            ]
        drag p1 p2 = nodeHandleEventEvts wenv (evtDrag p1 p2) node
        events evt = nodeHandleEventEvts wenv [evt] node
        expectedEvents = Seq.singleton $ ValueChanged (1, 0)
        focusEvents = Seq.singleton $ GotFocus emptyPath
        blurEvents = Seq.singleton $ LostFocus emptyPath
    it "should generate an event when it changes the board state" $
        drag (Point 5 5) (Point 325 5) `shouldBe` expectedEvents
    it "should generate an event when focus is received" $
        events evtFocus `shouldBe` focusEvents
    it "should generate an event when focus is lost" $
        events evtBlur `shouldBe` blurEvents

handleEventV :: Spec
handleEventV = describe "handleEventV" $ do
    let wenv = mockWenv $ TestModel [[42], []]
        node = dragboardV_ 2 1 [[42], []] onChangeEvent getColor
            [ onFocus GotFocus
            , onBlur LostFocus
            ]
        drag p1 p2 = nodeHandleEventEvts wenv (evtDrag p1 p2) node
        events evt = nodeHandleEventEvts wenv [evt] node
        expectedEvents = Seq.singleton $ ValueChanged (1, 0)
        focusEvents = Seq.singleton $ GotFocus emptyPath
        blurEvents = Seq.singleton $ LostFocus emptyPath
    it "should generate an event when it changes the board state" $
        drag (Point 5 5) (Point 325 5) `shouldBe` expectedEvents
    it "should generate an event when focus is received" $
        events evtFocus `shouldBe` focusEvents
    it "should generate an event when focus is lost" $
        events evtBlur `shouldBe` blurEvents

getColor :: Double -> Either Text Color
getColor = const $ Right white

onChangeEvent :: ([[Double]], Int, Int) -> Event
onChangeEvent (_, a, b) = ValueChanged (a, b)
