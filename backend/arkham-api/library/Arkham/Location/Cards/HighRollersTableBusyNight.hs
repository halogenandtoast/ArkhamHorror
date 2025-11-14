module Arkham.Location.Cards.HighRollersTableBusyNight (highRollersTableBusyNight) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Data.Function (on)

newtype HighRollersTableBusyNight = HighRollersTableBusyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

highRollersTableBusyNight :: LocationCard HighRollersTableBusyNight
highRollersTableBusyNight = symbolLabel $ location HighRollersTableBusyNight Cards.highRollersTableBusyNight 3 (PerPlayer 2)

instance HasAbilities HighRollersTableBusyNight where
  getAbilities (HighRollersTableBusyNight a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost (ResourceCost 4)

instance RunMessage HighRollersTableBusyNight where
  runMessage msg l@(HighRollersTableBusyNight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      checkGameIcons attrs iid (CanMulligan 1) 5
      pure l
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      cards' <- cards & mapMaybeM \c -> (c,) <$$> toPlayingCard c
      let hand = map fst $ sortBy (compare `on` snd) cards'
      whenM (orM [sequential cards, sameRank 4 cards, allSameSuit cards]) do
        winGame iid attrs 8
        remember WonACultistMedallion
      focusCards hand $ continue_ iid
      pure l
    _ -> HighRollersTableBusyNight <$> liftRunMessage msg attrs
