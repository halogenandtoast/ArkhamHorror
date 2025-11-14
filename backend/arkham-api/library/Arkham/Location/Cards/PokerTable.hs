module Arkham.Location.Cards.PokerTable (pokerTable) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Data.Function (on)

newtype PokerTable = PokerTable LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pokerTable :: LocationCard PokerTable
pokerTable = symbolLabel $ location PokerTable Cards.pokerTable 3 (PerPlayer 1)

instance HasAbilities PokerTable where
  getAbilities (PokerTable a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost (ResourceCost 2)

instance RunMessage PokerTable where
  runMessage msg l@(PokerTable attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      checkGameIcons attrs iid (CanMulligan 1) 5
      pure l
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      cards' <- cards & mapMaybeM \c -> (c,) <$$> toPlayingCard c
      let hand = map fst $ sortBy (compare `on` snd) cards'
      whenM (orM [sequential cards, sameRank 3 cards]) $ winGame iid attrs 5
      focusCards hand $ continue_ iid
      pure l
    _ -> PokerTable <$> liftRunMessage msg attrs
