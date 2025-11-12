module Arkham.Location.Cards.RouletteWheel (rouletteWheel) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype RouletteWheel = RouletteWheel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rouletteWheel :: LocationCard RouletteWheel
rouletteWheel = symbolLabel $ location RouletteWheel Cards.rouletteWheel 3 (PerPlayer 1)

instance HasAbilities RouletteWheel where
  getAbilities (RouletteWheel a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost (ResourceCost 1)

instance RunMessage RouletteWheel where
  runMessage msg l@(RouletteWheel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scenarioI18n
        $ chooseOneDropDown
          iid
          [(ikey' ("label.suit." <> tshow suit), DoStep (fromEnum suit) msg) | suit <- [minBound @Suit ..]]
      pure l
    DoStep _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      scenarioI18n
        $ chooseOneDropDown
          iid
          [(ikey' ("label.rank." <> tshow rank), DoStep (fromEnum rank) msg) | rank <- [minBound @Rank ..]]
      pure l
    DoStep rankN (DoStep suitN (UseThisAbility iid (isSource attrs -> True) 1)) -> do
      checkGameIcons attrs iid NoMulligan 1
      pure $ RouletteWheel $ attrs & setMeta (PlayingCard (toEnum rankN) (toEnum suitN))
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      cards' <- cards & mapMaybeM \c -> (c,) <$$> toPlayingCard c
      case cards' of
        ((c, pc) : _) -> do
          let pc' = toResult @PlayingCard attrs.meta
          when (pc == pc') $ winGame iid attrs 5
          focusCards [c] $ continue_ iid
        _ -> pure ()
      pure l
    _ -> RouletteWheel <$> liftRunMessage msg attrs
