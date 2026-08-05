module Arkham.Treachery.Cards.EyesOfYchlecht (eyesOfYchlecht) where

import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EyesOfYchlecht = EyesOfYchlecht TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyesOfYchlecht :: TreacheryCard EyesOfYchlecht
eyesOfYchlecht = treachery EyesOfYchlecht Cards.eyesOfYchlecht

instance RunMessage EyesOfYchlecht where
  runMessage msg t@(EyesOfYchlecht attrs) = runQueueT $ case msg of
    {- "Revelation - Test [willpower] (X), where X is Cthulhu's Rage. If you fail,
    take 1 direct horror and discard 1 card at random from your hand." Peril is on
    the card def. -}
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (ScenarioCount CthulhuRage)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directHorror iid attrs 1
      randomDiscard iid attrs
      pure t
    _ -> EyesOfYchlecht <$> liftRunMessage msg attrs
