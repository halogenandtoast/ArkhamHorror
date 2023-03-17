module Arkham.Helpers.Xp where

import Arkham.Prelude

import Arkham.Card.CardDef
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Arkham.Target

toGainXp :: HasGame m => m [(InvestigatorId, Int)] -> m [Message]
toGainXp f = map (uncurry GainXP) <$> f

getXp :: HasGame m => m [(InvestigatorId, Int)]
getXp = getXpWithBonus 0

getXpWithBonus :: (HasCallStack, HasGame m) => Int -> m [(InvestigatorId, Int)]
getXpWithBonus bonus = do
  investigatorIds <- allInvestigatorIds
  for
    investigatorIds
    \iid -> do
      modifiers' <- getModifiers (InvestigatorTarget iid)
      victoryPileVictory <- mconcat <$> scenarioFieldMap
        ScenarioVictoryDisplay
        (map (Sum . fromMaybe 0 . cdVictoryPoints . toCardDef))
      locationVictory <-
        fmap mconcat
        . traverse
            (fieldMap LocationCardDef (Sum . fromMaybe 0 . cdVictoryPoints))
        =<< selectList (RevealedLocation <> LocationWithoutClues)
      let amount = getSum $ victoryPileVictory <> locationVictory
      pure (iid, foldl' applyModifier (amount + bonus) modifiers')
 where
  applyModifier n (XPModifier m) = max 0 (n + m)
  applyModifier n _ = n
