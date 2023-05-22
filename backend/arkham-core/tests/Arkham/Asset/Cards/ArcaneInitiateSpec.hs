module Arkham.Asset.Cards.ArcaneInitiateSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types
import Arkham.Investigator.Types hiding (assetsL)
import Arkham.Matcher
import Arkham.Projection

spec :: Spec
spec = describe "Arcane Initiate" $ do
  it "enters play with 1 doom" $
    gameTest $ \investigator -> do
      putCardIntoPlay investigator Assets.arcaneInitiate
      chooseOnlyOption "trigger forced ability"
      arcaneInitiate <- selectJust $ assetIs Assets.arcaneInitiate
      assert $ fieldP AssetDoom (== 1) arcaneInitiate

  it "can be exhausted to search the top 3 cards of your deck for a Spell card" $
    gameTest $ \investigator -> do
      card <- genPlayerCard Assets.shrivelling
      otherCards <- testPlayerCards 2
      pushAndRun $ loadDeck investigator (card : otherCards)
      putCardIntoPlay investigator Assets.arcaneInitiate
      chooseOnlyOption "trigger forced ability"
      arcaneInitiate <- selectJust $ assetIs Assets.arcaneInitiate
      [_, ability] <- field AssetAbilities arcaneInitiate
      pushAndRun $ UseAbility (toId investigator) ability []
      chooseOnlyOption "search top of deck"
      chooseOnlyOption "take spell card"
      assert $ fieldP InvestigatorHand (== [PlayerCard card]) (toId investigator)

  it "should continue if no Spell card is found" $
    gameTest $ \investigator -> do
      cards <- testPlayerCards 3
      pushAndRun $ loadDeck investigator cards
      putCardIntoPlay investigator Assets.arcaneInitiate
      chooseOnlyOption "trigger forced ability"
      arcaneInitiate <- selectJust $ assetIs Assets.arcaneInitiate
      [_, ability] <- field AssetAbilities arcaneInitiate
      pushAndRun $ UseAbility (toId investigator) ability []
      chooseOnlyOption "search top of deck"
      chooseOptionMatching
        "no cards found"
        ( \case
            Label {} -> True
            _ -> False
        )
      assert $ fieldP InvestigatorHand null (toId investigator)
