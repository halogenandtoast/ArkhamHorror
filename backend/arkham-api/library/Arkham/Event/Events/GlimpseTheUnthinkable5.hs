module Arkham.Event.Events.GlimpseTheUnthinkable5 (glimpseTheUnthinkable5) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Strategy

newtype GlimpseTheUnthinkable5 = GlimpseTheUnthinkable5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseTheUnthinkable5 :: EventCard GlimpseTheUnthinkable5
glimpseTheUnthinkable5 =
  eventWith GlimpseTheUnthinkable5 Cards.glimpseTheUnthinkable5 (afterPlayL .~ RemoveThisFromGame)

instance RunMessage GlimpseTheUnthinkable5 where
  runMessage msg e@(GlimpseTheUnthinkable5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
      chooseAmount iid "Choose number of cards to discard" "$cards" 0 (length cards) attrs
      doStep 1 msg
      pure e
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      let n = getChoiceAmount "$cards" choices

      cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
      chooseNM iid n $ targets cards $ shuffleCardsIntoDeck iid . only
      pure e
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      handLimit <- field InvestigatorHandSize iid
      handCards <- fieldMap InvestigatorHand length iid
      drawCards iid attrs $ handLimit - handCards
      pure e
    _ -> GlimpseTheUnthinkable5 <$> liftRunMessage msg attrs
