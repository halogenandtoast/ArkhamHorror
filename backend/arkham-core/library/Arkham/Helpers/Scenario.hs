module Arkham.Helpers.Scenario where

import Arkham.Prelude

import Arkham.Campaign.Types
import Arkham.Card
import Arkham.Classes.Query
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario.Types
import Arkham.Token
import Control.Monad.Writer hiding ( filterM )
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE

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

toTokenValue :: ScenarioAttrs -> TokenFace -> Int -> Int -> TokenValue
toTokenValue attrs t esVal heVal = TokenValue
  t
  (NegativeModifier $ if isEasyStandard attrs then esVal else heVal)

isEasyStandard :: ScenarioAttrs -> Bool
isEasyStandard ScenarioAttrs { scenarioDifficulty } =
  scenarioDifficulty `elem` [Easy, Standard]

isHardExpert :: ScenarioAttrs -> Bool
isHardExpert ScenarioAttrs { scenarioDifficulty } =
  scenarioDifficulty `elem` [Hard, Expert]

getScenarioDeck :: HasGame m => ScenarioDeckKey -> m [Card]
getScenarioDeck k =
  scenarioFieldMap ScenarioDecks (HashMap.findWithDefault [] k)

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
