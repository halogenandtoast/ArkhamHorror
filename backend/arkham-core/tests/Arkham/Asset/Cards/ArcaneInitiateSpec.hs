module Arkham.Asset.Cards.ArcaneInitiateSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Arcane Initiate" $ do
  it "enters play with 1 doom" . gameTest $ \self -> do
    arcaneInitiate <- self `putAssetIntoPlay` Assets.arcaneInitiate
    useForcedAbility
    arcaneInitiate.doom `shouldReturn` 1

  it "can be exhausted to search the top 3 cards of your deck for a Spell card" . gameTest $ \self -> do
    card <- genPlayerCard Assets.shrivelling
    otherCards <- testPlayerCards 2
    withProp @"deck" (Deck $ card : otherCards) self
    arcaneInitiate <- self `putAssetIntoPlay` Assets.arcaneInitiate
    useForcedAbility
    [ability] <- self `getActionsFrom` arcaneInitiate
    self `useAbility` ability
    chooseOnlyOption "search top of deck"
    chooseOnlyOption "take spell card"
    assert arcaneInitiate.exhausted
    self.hand `shouldReturn` [toCard card]
    (unDeck <$> self.deck) `shouldMatchListM` otherCards

  it "should continue if no Spell card is found" . gameTest $ \self -> do
    cards <- testPlayerCards 3
    withProp @"deck" (Deck cards) self
    arcaneInitiate <- self `putAssetIntoPlay` Assets.arcaneInitiate
    useForcedAbility
    [ability] <- self `getActionsFrom` arcaneInitiate
    self `useAbility` ability
    chooseOnlyOption "search top of deck"
    chooseOptionMatching "no cards found" $ \case
      Label {} -> True
      _ -> False
    assert arcaneInitiate.exhausted
    self.hand `shouldReturn` []
    (unDeck <$> self.deck) `shouldMatchListM` cards
