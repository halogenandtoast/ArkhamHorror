module Arkham.Event.Events.Unflappable (unflappable) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Projection ()
import Arkham.Modifier

newtype Unflappable = Unflappable EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unflappable :: EventCard Unflappable
unflappable = event Unflappable Cards.unflappable

instance RunMessage Unflappable where
  runMessage msg e@(Unflappable attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #agility 2)
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      clues <- iid.clues
      when (clues >= 2) $ healHorror iid attrs 1
      pure e
    _ -> Unflappable <$> liftRunMessage msg attrs
