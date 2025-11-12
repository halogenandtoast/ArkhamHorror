module Arkham.Location.Cards.HighRollersTableCalmNight (highRollersTableCalmNight) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Data.Function (on)

newtype HighRollersTableCalmNight = HighRollersTableCalmNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRollersTableCalmNight :: LocationCard HighRollersTableCalmNight
highRollersTableCalmNight = symbolLabel $ location HighRollersTableCalmNight Cards.highRollersTableCalmNight 3 (PerPlayer 2)

instance HasAbilities HighRollersTableCalmNight where
  getAbilities (HighRollersTableCalmNight a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithoutClues)
      $ actionAbilityWithCost (ResourceCost 4)

instance RunMessage HighRollersTableCalmNight where
  runMessage msg l@(HighRollersTableCalmNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remember ImpersonatedAHighRoller
      checkGameIcons attrs iid CanMulligan 5
      pure l
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      let hand = sortBy (compare `on` toPlayingCard) $ filter (isJust . toPlayingCard) cards
      when (sequential cards || sameRank 4 cards || allSameSuit cards) $ winGame iid attrs 8
      focusCards hand $ continue_ iid
      pure l
    _ -> HighRollersTableCalmNight <$> liftRunMessage msg attrs
