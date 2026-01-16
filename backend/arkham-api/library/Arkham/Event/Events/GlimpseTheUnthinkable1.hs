module Arkham.Event.Events.GlimpseTheUnthinkable1 (glimpseTheUnthinkable1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype GlimpseTheUnthinkable1 = GlimpseTheUnthinkable1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseTheUnthinkable1 :: EventCard GlimpseTheUnthinkable1
glimpseTheUnthinkable1 = event GlimpseTheUnthinkable1 Cards.glimpseTheUnthinkable1

instance RunMessage GlimpseTheUnthinkable1 where
  runMessage msg e@(GlimpseTheUnthinkable1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawCards iid attrs 1
      doStep 1 msg
      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
      chooseAmount iid "Choose number of cards to discard" "$cards" 0 (length cards) attrs
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let n = getChoiceAmount "$cards" choices
      cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
      chooseNM iid n $ targets cards $ shuffleCardsIntoDeck iid . only
      drawCards iid attrs n
      pure e
    _ -> GlimpseTheUnthinkable1 <$> liftRunMessage msg attrs
