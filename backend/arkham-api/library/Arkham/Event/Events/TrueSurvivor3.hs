module Arkham.Event.Events.TrueSurvivor3 (trueSurvivor3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype TrueSurvivor3 = TrueSurvivor3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueSurvivor3 :: EventCard TrueSurvivor3
trueSurvivor3 = event TrueSurvivor3 Cards.trueSurvivor3

instance RunMessage TrueSurvivor3 where
  runMessage msg e@(TrueSurvivor3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ inDiscardOf iid <> basic (CardWithTrait Innate)
      when (null cards) (error "ScroungeForSupplies expected level 0 card in discard")
      chooseNM iid 3 $ targets cards (addToHand iid . only)
      pure e
    _ -> TrueSurvivor3 <$> liftRunMessage msg attrs
