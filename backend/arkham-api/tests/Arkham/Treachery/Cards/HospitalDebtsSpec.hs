module Arkham.Treachery.Cards.HospitalDebtsSpec (spec) where

import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Hospital Debts" $ do
  context "free ability" $ do
    it "can be used twice per round" . gameTest $ \self -> do
      self `gainResources` 3
      hospitalDebts <- self `putTreacheryIntoPlay` Treacheries.hospitalDebts
      duringRound $ do
        [ability1] <- self `getActionsFrom` hospitalDebts
        self `useAbility` ability1
        [ability2] <- self `getActionsFrom` hospitalDebts
        self `useAbility` ability2
        getActionsFrom self hospitalDebts `shouldReturn` []
      duringRound $ do
        getActionsFrom self hospitalDebts `shouldSatisfyM` ((== 1) . length)

    it "moves 1 resource to it" . gameTest $ \self -> do
      self `gainResources` 3
      hospitalDebts <- self `putTreacheryIntoPlay` Treacheries.hospitalDebts
      duringRound $ do
        [ability1] <- self `getActionsFrom` hospitalDebts
        self `useAbility` ability1
        [ability2] <- self `getActionsFrom` hospitalDebts
        self `useAbility` ability2
      hospitalDebts.resources `shouldReturn` 2
      self.resources `shouldReturn` 1

  context "forced ability" $ do
    context "with less than 6 resources on it" $ do
      it "causes you to earn 2 fewer experience" . gameTest $ \self -> do
        self `putCardIntoPlay` Treacheries.hospitalDebts
        run $ EndOfGame Nothing
        useForcedAbility
        getXpWithBonus 3 `shouldReturn` [(toId self, 1)]

    context "with 6 or more resources on it" $ do
      it "does not cause you to earn 2 fewer experience" . gameTest $ \self -> do
        hospitalDebts <- self `putTreacheryIntoPlay` Treacheries.hospitalDebts
        run $ PlaceTokens GameSource (toTarget hospitalDebts) #resource 6
        run $ EndOfGame Nothing
        self `getActionsFrom` hospitalDebts `shouldReturn` []
        getXpWithBonus 3 `shouldReturn` [(toId self, 3)]
