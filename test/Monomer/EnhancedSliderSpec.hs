{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.EnhancedSliderSpec (spec) where

import Control.Lens
import Data.Default
import Monomer
import Monomer.TestEventUtil
import Monomer.TestUtil
import Test.Hspec
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Monomer.Lens as L

import Monomer.EnhancedSlider
import Monomer.EnhancedSlider.UI (makeTitle)

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
    handleEventV
    testWidgetType
    configuration

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
    it "should decrease the value if it is not minimum" $
        model wenv ^. field `shouldBe` 40
    it "should not decrease the value if it is minimum" $
        model wenvM ^. field `shouldBe` 0

plusButton :: Spec
plusButton = describe "\"+\" button" $ do
    let wenv = mockWenvEvtUnit (TestModel 42)
        wenvM = mockWenvEvtUnit (TestModel 50)
        node = enhancedSlider_ field 0 50 [dragRate 2]
        p = Point (640-1) 50
        model wenv' = nodeHandleEventModel wenv' [evtClick p] node
    it "should increase the value if it is not maximum" $
        model wenv ^. field `shouldBe` 44
    it "should not increase the value if it is maximum" $
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

handleEventV :: Spec
handleEventV = describe "handleEventV" $ do
    let wenv = mockWenv (TestModel 42)
        node = enhancedSliderV_ 42 ValueChanged 0 50
            [ onFocus GotFocus
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

testWidgetType :: Spec
testWidgetType = describe "testWidgetType" $ do
    let node = enhancedSlider field 0 50 :: WidgetNode TestModel ()
    it "should set the correct widgetType" $ do
        let expectedType = "enhancedSlider-Double"
        node ^. L.info . L.widgetType `shouldBe` expectedType

configuration :: Spec
configuration = describe "configuration" $ do
    title
    labelHiding
    alignment

title :: Spec
title = describe "title" $ do
    let config = titleCaption "custom title"
        configM = titleMethod $ \x -> "x = " <> T.pack (show x)
        value = 42 :: Int
    it "should just show value by default" $
        makeTitle def value `shouldBe` "42"
    it "should show custom title with value" $
        makeTitle config value `shouldBe` "custom title: 42"
    it "should show custom title made by user provided method" $
        makeTitle configM value `shouldBe` "x = 42"

labelHiding :: Spec
labelHiding = describe "labelHiding" $ do
    let wenv = mockWenvEvtUnit (TestModel 42)
        node = enhancedSlider_ field 0 50 [hideLabel]
        p = Point (640-32-32-1) 5
        model = nodeHandleEventModel wenv [evtClick p] node
    it "should hide label" $
        model ^. field `shouldBe` 41

alignment :: Spec
alignment = describe "alignment" $ do
    let wenv = mockWenvEvtUnit (TestModel 42)
        model node p = nodeHandleEventModel wenv [evtClick p] node
    describe "horizontal" $ do
        it "should put slider to the left of the buttons" $ do
            let node = enhancedSlider_ field 0 50 [alignLeft]
                p = Point (640-32-32-1) 50
            model node p ^. field `shouldBe` 41
        it "should put slider between the buttons" $ do
            let node = enhancedSlider_ field 0 50 [alignCenter]
                p = Point 5 50
            model node p ^. field `shouldBe` 41
        it "should put slider to the right of the buttons" $ do
            let node = enhancedSlider_ field 0 50 [alignRight]
                p = Point 65 50
            model node p ^. field `shouldBe` 43
    describe "vertical" $ do
        it "should put slider to the top of the buttons" $ do
            let node = enhancedSlider_ field 0 50 [alignTop]
                p = Point 5 (480-5-24-24)
            model node p ^. field `shouldBe` 43
        it "should put slider between the buttons" $ do
            let node = enhancedSlider_ field 0 50 [alignMiddle]
                p = Point 5 50
            model node p ^. field `shouldBe` 43
        it "should put slider to the bottom of the buttons" $ do
            let node = enhancedSlider_ field 0 50 [alignBottom]
                p = Point 5 100
            model node p ^. field `shouldBe` 41
