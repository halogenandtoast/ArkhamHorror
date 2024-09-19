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
import Arkham.Name
import Arkham.Scenario.Types (Field (..))
import Arkham.Source
import Arkham.Xp

toGainXp :: (HasGame m, Sourceable source) => source -> m [(InvestigatorId, Int)] -> m [Message]
toGainXp (toSource -> source) f = map (\(iid, n) -> GainXP iid source n) <$> f

getXp :: HasGame m => m [(InvestigatorId, Int)]
getXp = getXpWithBonus 0

getXpWithBonus :: forall m. (HasCallStack, HasGame m) => Int -> m [(InvestigatorId, Int)]
getXpWithBonus bonus = do
  victoryPileVictory <- toVictory =<< getVictoryDisplay
  locationVictory <- toVictory =<< select (RevealedLocation <> LocationWithoutClues)
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

generateXpReport :: forall m. (HasCallStack, HasGame m) => Int -> m XpBreakdown
generateXpReport bonus = do
  victoryPileVictory <- fmap (map AllGainXp) . toVictory =<< scenarioField ScenarioVictoryDisplay
  locationVictory <-
    fmap (map AllGainXp) . toVictory =<< select (RevealedLocation <> LocationWithoutClues)
  investigatorIds <- allInvestigatorIds
  fromModifiers <- concatForM investigatorIds $ \iid -> do
    modifiers' <- getModifiers iid
    pure $ mapMaybe (modifierToXpDetail iid) modifiers'
  pure
    $ XpBreakdown
    $ [AllGainXp (XpDetail XpBonus "Bonus" bonus) | bonus > 0]
    <> victoryPileVictory
    <> locationVictory
    <> fromModifiers
 where
  modifierToXpDetail iid (XPModifier m) | m > 0 = Just (InvestigatorGainXp iid $ XpDetail XpFromCardEffect "Card Effect" m)
  modifierToXpDetail iid (XPModifier m) | m < 0 = Just (InvestigatorLoseXp iid $ XpDetail XpFromCardEffect "Card Effect" m)
  modifierToXpDetail _ _ = Nothing
  toVictory :: ConvertToCard c => [c] -> m [XpDetail]
  toVictory = mapMaybeM toEntry
  toEntry c = do
    card <- convertToCard c
    XpDetail XpFromVictoryDisplay (toTitle card) <$$> getVictoryPoints c
