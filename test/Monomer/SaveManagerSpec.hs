{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Monomer.SaveManagerSpec (spec) where

import Control.Lens
import Monomer
import Monomer.TestEventUtil
import Monomer.TestUtil
import Test.Hspec
import qualified Data.Sequence as Seq

import Monomer.SaveManager

data Event
    = SavesChanged (Saves Double)
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

buttons :: Spec
buttons = describe "buttons" $ do
    newSlotButton
    saveButton
    loadButton
    removeButton

newSlotButton :: Spec
newSlotButton = describe "New slot" $ do
    it "should make new save" $
        pendingWith "can't test async events"

saveButton :: Spec
saveButton = describe "Save" $ do
    it "should save into selected slot" $
        pendingWith "can't test async events"

loadButton :: Spec
loadButton = describe "Load" $ do
    let s = Seq.singleton (0, "a")
        m = initSaveManagerModel 42
            & savedData .~ s
            & selectedData .~ Just 0
        wenv = mockWenvEvtUnit $ TestModel m
        node = saveManager field
        p = Point ((640+16)/4*2+1) 5
        model = nodeHandleEventModel wenv [evtClick p] node
    it "should load from selected slot" $
        model ^. field . currentData `shouldBe` 0

removeButton :: Spec
removeButton = describe "Remove" $ do
    let s = Seq.singleton (0, "a")
        m = initSaveManagerModel 42
            & savedData .~ s
            & selectedData .~ Just 0
        wenv = mockWenvEvtUnit $ TestModel m
        node = saveManager field
        p = Point ((640+16)/4*3+1) 5
        model = nodeHandleEventModel wenv [evtClick p] node
    it "should remove selected slot" $
        model ^. field . savedData `shouldBe` Seq.empty

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
            ]
        clickEvents p = nodeHandleEventEvts wenv [evtClick p] node
        events evt = nodeHandleEventEvts wenv [evt] node
        expectedEvents = Seq.singleton $ SavesChanged Seq.empty
        focusEvents = Seq.singleton $ GotFocus emptyPath
        blurEvents = Seq.singleton $ LostFocus emptyPath
    it "should generate an event when saves change" $
        clickEvents (Point (640-2) 2) `shouldBe` expectedEvents
    it "should generate an event when focus is received" $
        events evtFocus `shouldBe` focusEvents
    it "should generate an event when focus is lost" $
        events evtBlur `shouldBe` blurEvents