module Arkham.Investigator.Cards.JoeDiamondSpec (spec) where

import Arkham.Card.Settings
import Arkham.Classes.HasGame (getGame)
import Arkham.Decklist.Type
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Investigator (getCardAttachments)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Deck qualified as InvestigatorDeck
import Arkham.Projection (fieldMap)
import Data.Map.Strict qualified as Map
import TestImport

spec :: Spec
spec = describe "Joe Diamond" do
  it "can use an imported arkham.build hunch deck during setup" $ gameTestWith Investigators.joeDiamond $ \self -> do
    withDeck self (importedHunchCards <> extraInsightCards)
    withImportedHunchDeck self importedHunchCodes

    run $ SetupInvestigator self.id
    chooseOptionMatching "use imported hunch deck" \case
      Label "$cards.label.joeDiamond.useImportedHunchDeck" _ -> True
      _ -> False

    decks <- fieldMap InvestigatorDecks id self.id
    let hunchDeckCodes = maybe [] (map toCardCode) $ Map.lookup InvestigatorDeck.HunchDeck decks
    liftIO $ hunchDeckCodes `shouldMatchList` importedHunchCodes

  it "can still manually build a hunch deck when an imported deck is configured" $ gameTestWith Investigators.joeDiamond $ \self -> do
    withDeck self (importedHunchCards <> extraInsightCards)
    withImportedHunchDeck self importedHunchCodes

    run $ SetupInvestigator self.id
    chooseOptionMatching "manually build hunch deck" \case
      Label "$cards.label.joeDiamond.manuallyBuildHunchDeck" _ -> True
      _ -> False

    questionMap <- gameQuestion <$> getGame
    case mapToList questionMap of
      [(_, question)] -> case stripQuestionWrappers question of
        ChooseN 10 msgs -> liftIO $ length msgs `shouldSatisfy` (>= 10)
        _ -> liftIO $ expectationFailure $ "expected hunch deck ChooseN question, got: " <> show question
      _ -> liftIO $ expectationFailure $ "expected one hunch deck question, got: " <> show questionMap

  it "replaces the imported hunch deck list when upgrading from arkham.build" $ gameTestWith Investigators.joeDiamond $ \self -> do
    withImportedHunchDeck self importedHunchCodes

    run $ UpgradeDecklist self.id upgradedHunchDecklist

    attachments <- getCardAttachments self.id Investigators.joeDiamond
    liftIO $ attachments `shouldBe` upgradedHunchCodes

withImportedHunchDeck :: Investigator -> [CardCode] -> TestAppT ()
withImportedHunchDeck self cardCodes = do
  current <- getInvestigator self.id
  void
    $ updateThis current \attrs ->
      attrs
        { investigatorSettings =
            updateCardSetting
              (toCardCode Investigators.joeDiamond)
              (SetCardSetting CardAttachments cardCodes)
              attrs.settings
        }

importedHunchCards :: [CardDef]
importedHunchCards =
  [ Events.unsolvedCase
  , Events.noStoneUnturned
  , Events.noStoneUnturned
  , Events.sceneOfTheCrime
  , Events.sceneOfTheCrime
  , Events.preposterousSketches
  , Events.preposterousSketches
  , Events.evidence
  , Events.evidence
  , Events.workingAHunch
  , Events.workingAHunch
  ]

importedHunchCodes :: [CardCode]
importedHunchCodes = map toCardCode importedHunchCards

upgradedHunchCards :: [CardDef]
upgradedHunchCards =
  [ Events.unsolvedCase
  , Events.noStoneUnturned
  , Events.noStoneUnturned
  , Events.sceneOfTheCrime
  , Events.sceneOfTheCrime
  , Events.preposterousSketches
  , Events.evidence
  , Events.evidence
  , Events.workingAHunch
  , Events.workingAHunch
  , Events.crypticResearch4
  ]

upgradedHunchCodes :: [CardCode]
upgradedHunchCodes = map toCardCode upgradedHunchCards

upgradedHunchDecklist :: ArkhamDBDecklist
upgradedHunchDecklist =
  ArkhamDBDecklist
    { slots = Map.fromListWith (+) [(toCardCode card, 1) | card <- upgradedHunchCards]
    , sideSlots = mempty
    , investigator_code = "05002"
    , investigator_name = "Joe Diamond"
    , meta = Just "{\"attachments_05002\":\"05010,03026,03026,04103,04103,02186,01022,01022,01037,01037,01043\"}"
    , taboo_id = Nothing
    , url = Nothing
    , decklist_id = Nothing
    , decklist_name = Nothing
    }

extraInsightCards :: [CardDef]
extraInsightCards =
  [ Events.logicalReasoning
  , Events.logicalReasoning
  ]
