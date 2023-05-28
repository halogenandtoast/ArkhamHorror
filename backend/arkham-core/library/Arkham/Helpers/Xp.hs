module Arkham.Helpers.Xp where

import Arkham.Prelude

import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Types (Field (..))
import Arkham.Source
import Arkham.Store

toGainXp :: (HasGame m, Sourceable source) => source -> m [(InvestigatorId, Int)] -> m [Message]
toGainXp (toSource -> source) f = map (\(iid, n) -> GainXP iid source n) <$> f

getXp :: (HasGame m, Store m Card) => m [(InvestigatorId, Int)]
getXp = getXpWithBonus 0

getXpWithBonus :: (HasCallStack, HasGame m, Store m Card) => Int -> m [(InvestigatorId, Int)]
getXpWithBonus bonus = do
  victoryPileVictory <- toVictory =<< scenarioField ScenarioVictoryDisplay
  locationVictory <-
    toVictory
      =<< selectList (RevealedLocation <> LocationWithoutClues)
  let initialAmount = bonus + getSum (victoryPileVictory <> locationVictory)
  investigatorIds <- allInvestigatorIds
  for investigatorIds $ \iid -> do
    modifiers' <- getModifiers iid
    pure (iid, foldl' applyModifier initialAmount modifiers')
 where
  applyModifier n (XPModifier m) = max 0 (n + m)
  applyModifier n _ = n
  toVictory :: (ConvertToCard c, HasGame m, Store m Card) => [c] -> m (Sum Int)
  toVictory = fmap (mconcat . map Sum . catMaybes) . traverse getVictoryPoints
