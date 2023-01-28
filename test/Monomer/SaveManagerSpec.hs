{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.SaveManagerSpec (spec) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.State
import Monomer
import Monomer.Main.WidgetTask
import Monomer.TestEventUtil
import Monomer.TestUtil
import Test.Hspec
import qualified Data.Sequence as Seq
import qualified Monomer.Lens as L

import Monomer.SaveManager

data Event
    = SavesChanged (Saves Double)
    | ValueChanged Double
    | GotFocus Path
    | LostFocus Path
    deriving (Eq, Show)

data TestModel = TestModel
    { _tmField :: SaveManagerModel Double
    } deriving Eq

makeLensesWith abbreviatedFields 'TestModel

spec :: Spec
spec = describe "SaveManager" $ do
    buttons
    handleEvent
    handleEventV

buttons :: Spec
buttons = describe "buttons" $ do
    newSlotButton
    saveButton
    loadButton
    removeButton

newSlotButton :: Spec
newSlotButton = describe "New slot" $ do
    let wenv = mockWenvEvtUnit $ TestModel $ initSaveManagerModel 42
        node = saveManager field
        es = [evtClick $ Point 5 5]
        stepCtx = nodeHandleEvents wenv WInit es node
        ((rqWenv, rqRoot, _), ctx) = stepCtx
    it "should make new save and select it" $ do
        let tasks = ctx ^. L.widgetTasks
        Seq.length tasks `shouldSatisfy` (>= 1)
        let WidgetProducer _ _ async = Seq.index tasks 0
        wait async
        ((wtWenv, _, _), _) <- flip runStateT ctx $
            handleWidgetTasks rqWenv rqRoot
        let savedData' = _weModel wtWenv ^. field . savedData
        Seq.length savedData' `shouldSatisfy` (>= 1)
        fst (Seq.index savedData' 0) `shouldBe` 42
        _weModel wtWenv ^. field . selectedData `shouldBe` Just 0

saveButton :: Spec
saveButton = describe "Save" $ do
    let s = Seq.singleton (0, "a")
        m = initSaveManagerModel 42
            & savedData .~ s
        m' = m & selectedData .~ Just 0
        wenv = mockWenvEvtUnit $ TestModel m
        wenvS = mockWenvEvtUnit $ TestModel m'
        node = saveManager field
        es = [evtClick $ Point ((640+16)/4+1) 5]
        stepCtx wenv' = nodeHandleEvents wenv' WInit es node
        ((rqWenv, rqRoot, _), ctx) = stepCtx wenv
        ((rqWenvS, rqRootS, _), ctxS) = stepCtx wenvS
    it "should not save when slot is not selected" $ do
        let tasks = ctx ^. L.widgetTasks
        Seq.length tasks `shouldBe` 0
        _weModel rqWenv ^. field . savedData `shouldBe` s
    it "should save into selected slot" $ do
        let tasks = ctxS ^. L.widgetTasks
        Seq.length tasks `shouldSatisfy` (>= 1)
        let WidgetProducer _ _ async = Seq.index tasks 0
        wait async
        ((wtWenvS, _, _), _) <- flip runStateT ctxS $
            handleWidgetTasks rqWenvS rqRootS
        let savedData' = _weModel wtWenvS ^. field . savedData
        Seq.length savedData' `shouldSatisfy` (>= 1)
        fst (Seq.index savedData' 0) `shouldBe` 42

loadButton :: Spec
loadButton = describe "Load" $ do
    let s = Seq.singleton (0, "a")
        m = initSaveManagerModel 42
            & savedData .~ s
        m' = m & selectedData .~ Just 0
        wenv = mockWenvEvtUnit $ TestModel m
        wenvS = mockWenvEvtUnit $ TestModel m'
        node = saveManager field
        p = Point ((640+16)/4*2+1) 5
        model wenv' = nodeHandleEventModel wenv' [evtClick p] node
    it "should not load when slot is not selected" $
        model wenv ^. field . currentData `shouldBe` 42
    it "should load from selected slot" $
        model wenvS ^. field . currentData `shouldBe` 0

removeButton :: Spec
removeButton = describe "Remove" $ do
    let s = Seq.singleton (0, "a")
        m = initSaveManagerModel 42
            & savedData .~ s
        m' = m & selectedData .~ Just 0
        wenv = mockWenvEvtUnit $ TestModel m
        wenvS = mockWenvEvtUnit $ TestModel m'
        node = saveManager field
        p = Point ((640+16)/4*3+1) 5
        model wenv' = nodeHandleEventModel wenv' [evtClick p] node
    it "should not remove when slot is not selected" $
        model wenv ^. field . savedData `shouldBe` s
    it "should remove selected slot" $
        model wenvS ^. field . savedData `shouldBe` Seq.empty

handleEvent :: Spec
handleEvent = describe "handleEvent" $ do
    let s = Seq.singleton (0, "a")
        m = initSaveManagerModel 42
            & savedData .~ s
            & selectedData .~ Just 0
        wenv = mockWenv $ TestModel m
        node = saveManager_ field
            [ onFocus GotFocus
            , onBlur LostFocus
            , onSavesChange SavesChanged
            , onChange ValueChanged
            ]
        clickEvents p = nodeHandleEventEvts wenv [evtClick p] node
        events evt = nodeHandleEventEvts wenv [evt] node
    it "should generate an event when saves change" $ do
        let p = Point (640-2) 2
            expectedEvents = Seq.singleton $ SavesChanged Seq.empty
        clickEvents p `shouldBe` expectedEvents
    it "should generate an event when current value is changed" $ do
        let p = Point ((640+16)/4*2+1) 5
            expectedEvents = Seq.singleton $ ValueChanged 0
        clickEvents p `shouldBe` expectedEvents
    it "should generate an event when focus is received" $ do
        let expectedEvents = Seq.singleton $ GotFocus emptyPath
        events evtFocus `shouldBe` expectedEvents
    it "should generate an event when focus is lost" $ do
        let expectedEvents = Seq.singleton $ LostFocus emptyPath
        events evtBlur `shouldBe` expectedEvents

handleEventV :: Spec
handleEventV = describe "handleEventV" $ do
    let s = Seq.singleton (0, "a")
        m = initSaveManagerModel 42
            & savedData .~ s
            & selectedData .~ Just 0
        wenv = mockWenv $ TestModel m
        node = saveManagerV_ m ValueChanged
            [ onFocus GotFocus
            , onBlur LostFocus
            , onSavesChange SavesChanged
            ]
        clickEvents p = nodeHandleEventEvts wenv [evtClick p] node
        events evt = nodeHandleEventEvts wenv [evt] node
    it "should generate an event when saves change" $ do
        let p = Point (640-2) 2
            expectedEvents = Seq.singleton $ SavesChanged Seq.empty
        clickEvents p `shouldBe` expectedEvents
    it "should generate an event when current value is changed" $ do
        let p = Point ((640+16)/4*2+1) 5
            expectedEvents = Seq.singleton $ ValueChanged 0
        clickEvents p `shouldBe` expectedEvents
    it "should generate an event when focus is received" $ do
        let expectedEvents = Seq.singleton $ GotFocus emptyPath
        events evtFocus `shouldBe` expectedEvents
    it "should generate an event when focus is lost" $ do
        let expectedEvents = Seq.singleton $ LostFocus emptyPath
        events evtBlur `shouldBe` expectedEvents
