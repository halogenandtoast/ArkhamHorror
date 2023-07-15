module Arkham.Asset.Cards.JimsTrumpetSpec (
  spec,
) where

import TestImport

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Token

spec :: Spec
spec = describe "Jim's Trumpet" $ do
  context "allows you to heal one horror when skull is revealed" $ do
    it "on yourself" $ gameTest $ \investigator -> do
      updateInvestigator investigator (Investigator.tokensL %~ setTokens Horror 1)
      putCardIntoPlay investigator Assets.jimsTrumpet
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Skull]
      pushAndRun $ moveTo investigator location
      pushAndRun $ beginSkillTest investigator SkillIntellect 0
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "choose self"
      fieldAssert InvestigatorHorror (== 0) investigator

    it "on an investigator at your location" $ gameTest $ \investigator -> do
      investigator2 <-
        addInvestigator
          Investigators.rolandBanks
          (Investigator.tokensL %~ setTokens Horror 1)
      putCardIntoPlay investigator Assets.jimsTrumpet
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Skull]
      pushAndRun $ moveAllTo location
      pushAndRun $ beginSkillTest investigator SkillIntellect 0
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "choose investigator at same location"
      fieldAssert InvestigatorHorror (== 0) investigator2

    it "even when another player draws token" $ gameTest $ \investigator -> do
      investigator2 <-
        addInvestigator
          Investigators.rolandBanks
          (Investigator.tokensL %~ setTokens Horror 1)
      putCardIntoPlay investigator Assets.jimsTrumpet
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Skull]
      pushAndRun $ moveAllTo location
      pushAndRun $ beginSkillTest investigator2 SkillIntellect 0
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "choose investigator at same location"
      fieldAssert InvestigatorHorror (== 0) investigator2

    it "on an investigator at a connected location" $ gameTest $ \investigator -> do
      investigator2 <-
        addInvestigator
          Investigators.rolandBanks
          (Investigator.tokensL %~ setTokens Horror 1)
      putCardIntoPlay investigator Assets.jimsTrumpet
      rivertown <- testLocationWithDef Locations.rivertown id
      southside <- testLocationWithDef Locations.southsideHistoricalSociety id
      pushAndRun $ SetChaosTokens [Skull]
      pushAndRun $ placedLocation rivertown
      pushAndRun $ placedLocation southside
      pushAndRun $ moveTo investigator rivertown
      pushAndRun $ moveTo investigator2 southside
      pushAndRun $ beginSkillTest investigator SkillIntellect 0
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "choose investigator at connected location"
      fieldAssert InvestigatorHorror (== 0) investigator2

    it "cannot target an investigator at an unconnected location" $ gameTest $ \investigator -> do
      investigator2 <-
        addInvestigator
          Investigators.rolandBanks
          ((Investigator.tokensL %~ setTokens Horror 1) . (Investigator.idL .~ "01001"))
      putCardIntoPlay investigator Assets.jimsTrumpet
      rivertown <- testLocationWithDef Locations.rivertown id
      downtown <- testLocationWithDef Locations.downtownArkhamAsylum id
      pushAndRun $ SetChaosTokens [Skull]
      pushAndRun $ placedLocation rivertown
      pushAndRun $ placedLocation downtown
      pushAndRun $ moveTo investigator rivertown
      pushAndRun $ moveTo investigator2 downtown
      pushAndRun $ beginSkillTest investigator SkillIntellect 0
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      fieldAssert InvestigatorHorror (== 1) investigator2
