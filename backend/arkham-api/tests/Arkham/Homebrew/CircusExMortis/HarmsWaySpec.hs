module Arkham.Homebrew.CircusExMortis.HarmsWaySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Story
import Arkham.Projection (field)
import Data.Text qualified as T
import TestImport

spec :: Spec
spec = describe "Harm's Way opening story cards" do
  it "reserves both earned cards before opening hands and adds them after the mulligan"
    . scenarioTest ":circus-ex-mortis:040"
    $ \self -> do
      withDeck self (replicate 10 Assets.knife)
      addCampaignCardToDeck Story.amaltheaWeaverCircusFortuneTeller
      addCampaignCardToDeck Story.deCultusBestiaeForgottenWorkOfApuleius
      overTest (inSetupL .~ True)
      run PreScenarioSetup
      chooseFirstOption "intro"
      chooseFirstOption "start with Amalthea"
      chooseFirstOption "start with the tome"

      deck <- field InvestigatorDeck self.id
      liftIO $ map toCardCode (unDeck deck) `shouldMatchList` replicate 10 (toCardCode Assets.knife)
      run $ SetupInvestigator self.id
      run $ DrawStartingHand self.id
      run $ FinishedWithMulligan self.id
      hand <- field InvestigatorHand self.id
      liftIO
        $ map toCardCode hand
        `shouldMatchList` ( replicate 5 (toCardCode Assets.knife)
                              <> map
                                toCardCode
                                [ Story.amaltheaWeaverCircusFortuneTeller
                                , Story.deCultusBestiaeForgottenWorkOfApuleius
                                ]
                          )

  it "lets the investigator leave each earned card in the deck"
    . scenarioTest ":circus-ex-mortis:040"
    $ \self -> do
      withDeck self (replicate 10 Assets.knife)
      addCampaignCardToDeck Story.amaltheaWeaverCircusFortuneTeller
      addCampaignCardToDeck Story.deCultusBestiaeForgottenWorkOfApuleius
      overTest (inSetupL .~ True)
      run PreScenarioSetup
      chooseFirstOption "intro"
      let leave = chooseOptionMatching "leave story card in deck" \case
            Label label _ -> "leave" `T.isInfixOf` label
            _ -> False
      leave
      leave
      deck <- field InvestigatorDeck self.id
      liftIO
        $ map toCardCode (unDeck deck)
        `shouldMatchList` ( replicate 10 (toCardCode Assets.knife)
                              <> map
                                toCardCode
                                [ Story.amaltheaWeaverCircusFortuneTeller
                                , Story.deCultusBestiaeForgottenWorkOfApuleius
                                ]
                          )
