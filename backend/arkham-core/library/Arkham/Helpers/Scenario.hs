module Arkham.Helpers.Scenario where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Query
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Matcher
import Arkham.PlayerCard
import Arkham.Projection
import Arkham.Scenario.Attrs
import Arkham.Token
import Control.Monad.Writer hiding ( filterM )
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE

scenarioField :: Field ScenarioAttrs a -> GameT a
scenarioField fld = scenarioFieldMap fld id

scenarioFieldMap :: Field ScenarioAttrs a -> (a -> b) -> GameT b
scenarioFieldMap fld f = selectJust TheScenario >>= fieldMap fld f

getIsStandalone :: GameT Bool
getIsStandalone = isNothing <$> selectOne TheCampaign

addRandomBasicWeaknessIfNeeded
  :: Deck PlayerCard -> GameT (Deck PlayerCard, [CardDef])
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

getScenarioDeck :: ScenarioDeckKey -> GameT [Card]
getScenarioDeck k = scenarioFieldMap ScenarioDecks (HashMap.findWithDefault [] k)
