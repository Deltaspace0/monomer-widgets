{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.EnhancedSliderSpec (spec) where

import Control.Lens
import Monomer
import Monomer.TestEventUtil
import Monomer.TestUtil
import Test.Hspec
import qualified Data.Sequence as Seq

import Monomer.EnhancedSlider

data Event
    = ValueChanged Double
    | GotFocus Path
    | LostFocus Path
    deriving (Eq, Show)

data TestModel = TestModel
    { _tmField :: Double
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'TestModel

spec :: Spec
spec = describe "EnhancedSlider" $ do
    buttons
    handleEvent

buttons :: Spec
buttons = describe "buttons" $ do
    minusButton
    plusButton

minusButton :: Spec
minusButton = describe "\"-\" button" $ do
    let wenv = mockWenvEvtUnit (TestModel 42)
        wenvM = mockWenvEvtUnit (TestModel 0)
        node = enhancedSlider_ field 0 50 [dragRate 2]
        p = Point (640-32-32-1) 50
        model wenv' = nodeHandleEventModel wenv' [evtClick p] node
    it "should decrease the value if it's not minimum" $
        model wenv ^. field `shouldBe` 40
    it "shouldn't decrease the value if it's minimum" $
        model wenvM ^. field `shouldBe` 0

plusButton :: Spec
plusButton = describe "\"+\" button" $ do
    let wenv = mockWenvEvtUnit (TestModel 42)
        wenvM = mockWenvEvtUnit (TestModel 50)
        node = enhancedSlider_ field 0 50 [dragRate 2]
        p = Point (640-1) 50
        model wenv' = nodeHandleEventModel wenv' [evtClick p] node
    it "should increase the value if it's not maximum" $
        model wenv ^. field `shouldBe` 44
    it "shouldn't increase the value if it's maximum" $
        model wenvM ^. field `shouldBe` 50

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
    let wenv = mockWenv (TestModel 42)
        node = enhancedSlider_ field 0 50
            [ onChange ValueChanged
            , onFocus GotFocus
            , onBlur LostFocus
            ]
        clickEvents p = nodeHandleEventEvts wenv [evtClick p] node
        events evt = nodeHandleEventEvts wenv [evt] node
        expectedEvents = Seq.singleton $ ValueChanged 43
        focusEvents = Seq.singleton $ GotFocus emptyPath
        blurEvents = Seq.singleton $ LostFocus emptyPath
    it "should generate an event when it changes the value" $
        clickEvents (Point (640-1) 50) `shouldBe` expectedEvents
    it "should generate an event when focus is received" $
        events evtFocus `shouldBe` focusEvents
    it "should generate an event when focus is lost" $
        events evtBlur `shouldBe` blurEvents
