{-# OPTIONS_GHC -Wno-dodgy-imports #-}

module Arkham.Helpers.Scenario where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.Query
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario.Types
import Arkham.Target
import Control.Lens (non, _1, _2)
import Control.Monad.Writer hiding (filterM)
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

whenStandalone :: HasGame m => m () -> m ()
whenStandalone = whenM getIsStandalone

addRandomBasicWeaknessIfNeeded
  :: MonadRandom m => Deck PlayerCard -> m (Deck PlayerCard, [CardDef])
addRandomBasicWeaknessIfNeeded deck = runWriterT $ do
  Deck <$> flip
    filterM
    (unDeck deck)
    \card -> do
      when
        (toCardDef card == randomWeakness)
        (sample (NE.fromList allBasicWeaknesses) >>= tell . pure)
      pure $ toCardDef card /= randomWeakness

toChaosTokenValue :: ScenarioAttrs -> ChaosTokenFace -> Int -> Int -> ChaosTokenValue
toChaosTokenValue attrs t esVal heVal =
  ChaosTokenValue
    t
    (NegativeModifier $ if isEasyStandard attrs then esVal else heVal)

isEasyStandard :: ScenarioAttrs -> Bool
isEasyStandard ScenarioAttrs {scenarioDifficulty} =
  scenarioDifficulty `elem` [Easy, Standard]

isHardExpert :: ScenarioAttrs -> Bool
isHardExpert ScenarioAttrs {scenarioDifficulty} =
  scenarioDifficulty `elem` [Hard, Expert]

getScenarioDeck :: HasGame m => ScenarioDeckKey -> m [Card]
getScenarioDeck k =
  scenarioFieldMap ScenarioDecks (Map.findWithDefault [] k)

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
  let onlyPlayerCards = mapMaybe (preview _PlayerCard)
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
