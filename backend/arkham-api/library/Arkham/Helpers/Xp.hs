module Arkham.Helpers.Xp where

import Arkham.Prelude

import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers.Card
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Types (Field (..))
import Arkham.Source

toGainXp :: (HasGame m, Sourceable source) => source -> m [(InvestigatorId, Int)] -> m [Message]
toGainXp (toSource -> source) f = map (\(iid, n) -> GainXP iid source n) <$> f

getXp :: HasGame m => m [(InvestigatorId, Int)]
getXp = getXpWithBonus 0

getXpWithBonus :: forall m. (HasCallStack, HasGame m) => Int -> m [(InvestigatorId, Int)]
getXpWithBonus bonus = do
  victoryPileVictory <- toVictory =<< scenarioField ScenarioVictoryDisplay
  locationVictory <-
    toVictory
      =<< select (RevealedLocation <> LocationWithoutClues)
  let initialAmount = bonus + getSum (victoryPileVictory <> locationVictory)
  investigatorIds <- allInvestigatorIds
  for investigatorIds $ \iid -> do
    modifiers' <- getModifiers iid
    pure (iid, foldl' applyModifier initialAmount modifiers')
 where
  applyModifier n (XPModifier m) = max 0 (n + m)
  applyModifier n _ = n
  toVictory :: ConvertToCard c => [c] -> m (Sum Int)
  toVictory = fmap (mconcat . map Sum . catMaybes) . traverse getVictoryPoints
