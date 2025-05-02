module Arkham.Treachery.Cards.GlimpseTheUnspeakable (glimpseTheUnspeakable) where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Modifier
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GlimpseTheUnspeakable = GlimpseTheUnspeakable TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseTheUnspeakable :: TreacheryCard GlimpseTheUnspeakable
glimpseTheUnspeakable = treachery GlimpseTheUnspeakable Cards.glimpseTheUnspeakable

instance RunMessage GlimpseTheUnspeakable where
  runMessage msg t@(GlimpseTheUnspeakable attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      cards <- getTekelili 1
      when (null cards) $ gainSurge attrs
      for_ cards \card -> do
        cardResolutionModifier card attrs card.id ShuffleIntoAnyDeckInsteadOfDiscard
        drawCardFrom iid card TekeliliDeck
      pure t
    _ -> GlimpseTheUnspeakable <$> liftRunMessage msg attrs
