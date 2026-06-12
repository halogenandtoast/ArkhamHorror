module Arkham.Scenarios.EnthrallingEncore.Helpers where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Campaign.Types (Field (CampaignDecks))
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.ClassSymbol
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import Arkham.Event.Types (Field (EventCard))
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Campaign (campaignField, getCampaignStoryCards)
import Arkham.Helpers.Scenario (getScenarioDeck, scenarioFieldMap, standaloneI18n)
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (
  Field (InvestigatorDeck, InvestigatorDiscard, InvestigatorHand, InvestigatorName),
 )
import Arkham.Matcher hiding (AssetCard, EventCard, SkillCard)
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (ScenarioTokens))
import Arkham.Skill.Types (Field (SkillCard))
import Arkham.Token
import Arkham.Tracing
import Arkham.Treachery.Cards qualified as Treacheries

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "enthrallingEncore" a

getMeasures :: (HasGame m, Tracing m) => m Int
getMeasures = scenarioFieldMap ScenarioTokens (countTokens Resource)

getPropsDeck :: (HasGame m, Tracing m) => m [Card]
getPropsDeck = getScenarioDeck PropsDeck

propsDeckCards :: [CardDef]
propsDeckCards =
  [ Assets.physicalTraining
  , Assets.firstAid
  , Assets.machete
  , Assets.oldBookOfLore
  , Assets.hyperawareness
  , Assets.medicalTexts
  , Assets.burglary
  , Assets.pickpocketing
  , Assets.hardKnocks
  , Assets.holyRosary
  , Assets.scrying
  , Assets.arcaneStudies
  , Assets.baseballBat
  , Assets.rabbitsFoot
  , Assets.digDeep
  ]

classesAmongControlledCards :: (HasGame m, Tracing m) => InvestigatorId -> m Int
classesAmongControlledCards iid = do
  assets <- traverse (field AssetCard) =<< select (assetControlledBy iid)
  events <- traverse (field EventCard) =<< select (eventControlledBy iid)
  skills <- traverse (field SkillCard) =<< select (SkillControlledBy $ InvestigatorWithId iid)
  hand <- field InvestigatorHand iid
  deck <- fieldMap InvestigatorDeck (map toCard . unDeck) iid
  discard <- fieldMap InvestigatorDiscard (map toCard) iid
  pure
    . length
    . nub
    . filter (`elem` [Guardian, Seeker, Rogue, Mystic, Survivor])
    $ concatMap (toList . cdClassSymbols . toCardDef) (assets <> events <> skills <> hand <> deck <> discard)

data SignatureSwap = SignatureSwap
  { signatureCard :: CardDef
  , advancedSignatureCard :: CardDef
  , weaknessCard :: CardDef
  , advancedWeaknessCard :: CardDef
  }

signatureSwaps :: Map Text SignatureSwap
signatureSwaps =
  mapFromList
    [ ( "Roland Banks"
      , SignatureSwap Assets.rolands38Special Assets.rolands38SpecialAdvanced Treacheries.coverUp Treacheries.coverUpAdvanced
      )
    , ( "Daisy Walker"
      , SignatureSwap Assets.daisysToteBag Assets.daisysToteBagAdvanced Assets.theNecronomicon Assets.theNecronomiconAdvanced
      )
    , ( "\"Skids\" O'Toole"
      , SignatureSwap Events.onTheLam Events.onTheLamAdvanced Treacheries.hospitalDebts Treacheries.hospitalDebtsAdvanced
      )
    , ( "Agnes Baker"
      , SignatureSwap Assets.heirloomOfHyperborea Assets.heirloomOfHyperboreaAdvanced Events.darkMemory Events.darkMemoryAdvanced
      )
    , ( "Wendy Adams"
      , SignatureSwap Assets.wendysAmulet Assets.wendysAmuletAdvanced Treacheries.abandonedAndAlone Treacheries.abandonedAndAloneAdvanced
      )
    , ( "Zoey Samaras"
      , SignatureSwap Assets.zoeysCross Assets.zoeysCrossAdvanced Treacheries.smiteTheWicked Treacheries.smiteTheWickedAdvanced
      )
    , ( "Rex Murphy"
      , SignatureSwap Events.searchForTheTruth Events.searchForTheTruthAdvanced Treacheries.rexsCurse Treacheries.rexsCurseAdvanced
      )
    , ( "Jenny Barnes"
      , SignatureSwap Assets.jennysTwin45s Assets.jennysTwin45sAdvanced Treacheries.searchingForIzzie Treacheries.searchingForIzzieAdvanced
      )
    , ( "Jim Culver"
      , SignatureSwap Assets.jimsTrumpet Assets.jimsTrumpetAdvanced Treacheries.finalRhapsody Treacheries.finalRhapsodyAdvanced
      )
    , ( "Father Mateo"
      , SignatureSwap Assets.theCodexOfAges Assets.theCodexOfAgesAdvanced Enemies.serpentsOfYig Enemies.serpentsOfYigAdvanced
      )
    , ( "Monterey Jack"
      , SignatureSwap Assets.trustyBullwhip Assets.trustyBullwhipAdvanced Treacheries.buriedSecrets Treacheries.buriedSecretsAdvanced
      )
    ]

getSignatureSwap :: (HasGame m, Tracing m) => InvestigatorId -> m (Maybe SignatureSwap)
getSignatureSwap iid = do
  title <- fieldMap InvestigatorName toTitle iid
  pure $ lookup title signatureSwaps

getOwnedCardDefs :: (HasGame m, Tracing m) => InvestigatorId -> m [CardDef]
getOwnedCardDefs iid =
  selectOne TheCampaign >>= \case
    Just _ -> do
      decks <- campaignField CampaignDecks
      storyCards <- getCampaignStoryCards
      pure
        $ map toCardDef (maybe [] unDeck $ lookup iid decks)
        <> map toCardDef (findWithDefault [] iid storyCards)
    Nothing -> do
      deck <- fieldMap InvestigatorDeck unDeck iid
      hand <- field InvestigatorHand iid
      discard <- field InvestigatorDiscard iid
      assets <- traverse (field AssetCard) =<< select (assetControlledBy iid)
      props <- getPropsDeck
      pure
        $ map toCardDef deck
        <> map toCardDef hand
        <> map toCardDef discard
        <> map toCardDef assets
        <> map toCardDef props
