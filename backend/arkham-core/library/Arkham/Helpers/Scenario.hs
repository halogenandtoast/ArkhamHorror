module Arkham.Helpers.Scenario where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Query
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Id
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
scenarioFieldMap fld f = selectJust TheScenario >>= fieldMap fld f

getIsStandalone :: HasGame m => m Bool
getIsStandalone = isNothing <$> selectOne TheCampaign

addRandomBasicWeaknessIfNeeded
  :: MonadRandom m => Deck PlayerCard -> m (Deck PlayerCard, [SomeCardDef])
addRandomBasicWeaknessIfNeeded deck = runWriterT $ do
  Deck <$> flip
    filterM
    (unDeck deck)
    \card -> do
      when
        (toCardDef card == SomeCardDef SPlayerTreacheryType randomWeakness)
        (sample (NE.fromList allBasicWeaknesses) >>= tell . pure)
      pure $ toCardDef card /= SomeCardDef SPlayerTreacheryType randomWeakness

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

resignedWith :: HasGame m => SomeCardDef -> m Bool
resignedWith cDef =
  scenarioFieldMap ScenarioResignedCardCodes (elem (withCardDef toCardCode cDef))
