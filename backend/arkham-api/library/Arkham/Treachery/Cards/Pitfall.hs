module Arkham.Treachery.Cards.Pitfall (pitfall) where

import Arkham.Deck
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Pitfall = Pitfall TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pitfall :: TreacheryCard Pitfall
pitfall = treachery Pitfall Cards.pitfall

instance RunMessage Pitfall where
  runMessage msg t@(Pitfall attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseOrRunOneM iid do
        labeled "Test {agility} (3) to attempt to jump the gap. For each point you fail by, take 1 damage."
          $ revelationSkillTest sid iid attrs #agility (Fixed 3)

        when (attrs.drawnFrom /= Just (ScenarioDeckByKey ExplorationDeck)) do
          labeled
            "Shuffle Pitfall into the exploration deck. You cannot choose this option if Pitfall was drawn from the exploration deck."
            $ shuffleIntoDeck ExplorationDeck attrs
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid attrs n
      pure t
    _ -> Pitfall <$> liftRunMessage msg attrs
