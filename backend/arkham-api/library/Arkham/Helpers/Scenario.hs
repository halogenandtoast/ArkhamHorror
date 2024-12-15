module Arkham.Helpers.Scenario where

import Arkham.Prelude

import Arkham.Calculation
import Arkham.Campaign.Types
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Layout
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario.Types
import Arkham.Target
import Control.Lens (non, _1, _2)
import Control.Monad.Writer
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map

scenarioField :: (HasCallStack, HasGame m) => Field Scenario a -> m a
scenarioField fld = scenarioFieldMap fld id

scenarioFieldMap
  :: (HasCallStack, HasGame m) => Field Scenario a -> (a -> b) -> m b
scenarioFieldMap fld f = scenarioFieldMapM fld (pure . f)

scenarioFieldMapM
  :: (HasCallStack, HasGame m) => Field Scenario a -> (a -> m b) -> m b
scenarioFieldMapM fld f = selectJust TheScenario >>= fieldMapM fld f

getIsStandalone :: HasGame m => m Bool
getIsStandalone = isNothing <$> selectOne TheCampaign

getEncounterDeck :: HasGame m => m (Deck EncounterCard)
getEncounterDeck = scenarioField ScenarioEncounterDeck

getVictoryDisplay :: HasGame m => m [Card]
getVictoryDisplay = scenarioField ScenarioVictoryDisplay

inVictoryDisplay :: HasGame m => CardMatcher -> m Bool
inVictoryDisplay matcher = any (`cardMatch` matcher) <$> getVictoryDisplay

whenStandalone :: HasGame m => m () -> m ()
whenStandalone = whenM getIsStandalone

unlessStandalone :: HasGame m => m () -> m ()
unlessStandalone = unlessM getIsStandalone

addRandomBasicWeaknessIfNeeded
  :: MonadRandom m => ClassSymbol -> Int -> Deck PlayerCard -> m (Deck PlayerCard, [CardDef])
addRandomBasicWeaknessIfNeeded investigatorClass playerCount deck = do
  let
    multiplayerFilter =
      if playerCount < 2
        then notElem MultiplayerOnly . cdDeckRestrictions
        else const True
    notForClass = \case
      OnlyClass c -> c /= investigatorClass
      _ -> True
    classOnlyFilter = not . any notForClass . cdDeckRestrictions
    weaknessFilter = and . sequence [multiplayerFilter, classOnlyFilter]
  runWriterT $ do
    Deck <$> flip
      filterM
      (unDeck deck)
      \card -> do
        when
          (toCardDef card == randomWeakness)
          (sample (NE.fromList $ filter weaknessFilter nonCampaignOnlyWeaknesses) >>= tell . pure)
        pure $ toCardDef card /= randomWeakness
 where
  nonCampaignOnlyWeaknesses =
    filter (not . isCampaignOnly) allBasicWeaknesses
  isCampaignOnly = elem CampaignModeOnly . cdDeckRestrictions

toChaosTokenValue :: ScenarioAttrs -> ChaosTokenFace -> Int -> Int -> ChaosTokenValue
toChaosTokenValue attrs t esVal heVal =
  ChaosTokenValue
    t
    (CalculatedModifier $ Negated $ Fixed $ if isEasyStandard attrs then esVal else heVal)

byDifficulty :: ScenarioAttrs -> a -> a -> a
byDifficulty attrs a b = if isEasyStandard attrs then a else b

isEasyStandard :: ScenarioAttrs -> Bool
isEasyStandard ScenarioAttrs {scenarioDifficulty} =
  scenarioDifficulty `elem` [Easy, Standard]

isHardExpert :: ScenarioAttrs -> Bool
isHardExpert ScenarioAttrs {scenarioDifficulty} =
  scenarioDifficulty `elem` [Hard, Expert]

getScenarioDeck :: HasGame m => ScenarioDeckKey -> m [Card]
getScenarioDeck k =
  scenarioFieldMap ScenarioDecks (Map.findWithDefault [] k)

getScenarioMeta :: (HasCallStack, HasGame m, FromJSON a) => m a
getScenarioMeta = scenarioFieldMap ScenarioMeta toResult

getEncounterDiscard :: HasGame m => ScenarioEncounterDeckKey -> m [EncounterCard]
getEncounterDiscard RegularEncounterDeck = scenarioField ScenarioDiscard
getEncounterDiscard k =
  scenarioFieldMap ScenarioEncounterDecks (view (at k . non (Deck [], []) . _2))

withStandalone
  :: HasGame m => (CampaignId -> m a) -> (ScenarioId -> m a) -> m a
withStandalone cf sf =
  maybe (sf =<< selectJust TheScenario) cf =<< selectOne TheCampaign

resignedWith :: HasGame m => CardDef -> m Bool
resignedWith cDef =
  scenarioFieldMap ScenarioResignedCardCodes (elem (toCardCode cDef))

findTopOfDiscard :: HasGame m => CardMatcher -> m (Maybe EncounterCard)
findTopOfDiscard = fmap listToMaybe . findInDiscard

findInDiscard :: HasGame m => CardMatcher -> m [EncounterCard]
findInDiscard matcher =
  scenarioFieldMap ScenarioDiscard (filter (`cardMatch` matcher))

getOriginalDeck :: HasGame m => InvestigatorId -> m (Deck PlayerCard)
getOriginalDeck iid = do
  dict <- withStandalone (field CampaignDecks) (field ScenarioPlayerDecks)
  pure $ findWithDefault mempty iid dict

getKnownRemainingOriginalDeckCards
  :: HasGame m => InvestigatorId -> m [PlayerCard]
getKnownRemainingOriginalDeckCards iid = do
  cards <- unDeck <$> getOriginalDeck iid
  inDiscard <- field InvestigatorDiscard iid
  inHand <- fieldMap InvestigatorHand onlyPlayerCards iid
  inVictory <- scenarioFieldMap ScenarioVictoryDisplay onlyPlayerCards
  let knownNotInDeck = inDiscard <> inHand <> inVictory
  pure $ filter (`notElem` knownNotInDeck) cards

isInVictoryDisplay :: HasGame m => CardDef -> m Bool
isInVictoryDisplay def = scenarioFieldMap ScenarioVictoryDisplay ((elem def) . map toCardDef)

data EncounterDeckHandler = EncounterDeckHandler
  { deckLens :: Lens' ScenarioAttrs (Deck EncounterCard)
  , discardLens :: Lens' ScenarioAttrs [EncounterCard]
  }

getEncounterDeckKey :: (HasGame m, Targetable a) => a -> m ScenarioEncounterDeckKey
getEncounterDeckKey a = do
  modifiers' <- getModifiers a
  pure $ fromMaybe RegularEncounterDeck $ asum $ map toEncounterDeckModifier modifiers'

getEncounterDeckHandler :: (HasGame m, Targetable a) => a -> m EncounterDeckHandler
getEncounterDeckHandler a = do
  key <- getEncounterDeckKey a
  pure $ case key of
    RegularEncounterDeck ->
      EncounterDeckHandler
        { deckLens = encounterDeckLensFromKey RegularEncounterDeck
        , discardLens = discardL
        }
    other ->
      EncounterDeckHandler
        { deckLens = encounterDeckLensFromKey other
        , discardLens = encounterDecksL . at other . non (Deck [], []) . _2
        }

toEncounterDeckModifier :: ModifierType -> Maybe ScenarioEncounterDeckKey
toEncounterDeckModifier (UseEncounterDeck k) = Just k
toEncounterDeckModifier _ = Nothing

encounterDeckLensFromKey :: ScenarioEncounterDeckKey -> Lens' ScenarioAttrs (Deck EncounterCard)
encounterDeckLensFromKey RegularEncounterDeck = encounterDeckL
encounterDeckLensFromKey k = encounterDecksL . at k . non (Deck [], []) . _1

getGrid :: HasGame m => m Grid
getGrid = scenarioField ScenarioGrid

getLayout :: HasGame m => m [GridTemplateRow]
getLayout = scenarioField ScenarioLocationLayout

guardInScenario :: HasGame m => MaybeT m ()
guardInScenario = liftGuardM inScenario

inScenario :: HasGame m => m Bool
inScenario = selectAny TheScenario
