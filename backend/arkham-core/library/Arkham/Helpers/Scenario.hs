module Arkham.Helpers.Scenario where

import Arkham.Prelude

import Arkham.Classes.Query
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Attrs

scenarioField
  :: (Query ScenarioMatcher m, Projection m ScenarioAttrs)
  => Field ScenarioAttrs a
  -> m a
scenarioField fld = scenarioFieldMap fld id

scenarioFieldMap
  :: (Monad m, Query ScenarioMatcher m, Projection m ScenarioAttrs)
  => Field ScenarioAttrs a
  -> (a -> b)
  -> m b
scenarioFieldMap fld f = selectJust TheScenario >>= fieldMap fld f

getIsStandalone :: GameT Bool
getIsStandalone = isNothing <$> selectOne TheCampaign

addRandomBasicWeaknessIfNeeded :: Deck PlayerCard -> GameT (Deck PlayerCard, [CardDef])
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

