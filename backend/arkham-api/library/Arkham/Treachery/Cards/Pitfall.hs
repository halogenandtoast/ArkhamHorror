module Arkham.Treachery.Cards.Pitfall (pitfall) where

import Arkham.Deck
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.HeartOfTheElders.Helpers
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
      chooseOrRunOneM iid $ scenarioI18n do
        labeled' "pitfall.jumpTheGap" $ revelationSkillTest sid iid attrs #agility (Fixed 3)

        when (attrs.drawnFrom /= Just (ScenarioDeckByKey ExplorationDeck)) do
          labeled' "pitfall.shuffle" $ shuffleIntoDeck ExplorationDeck attrs
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid attrs n
      pure t
    _ -> Pitfall <$> liftRunMessage msg attrs
