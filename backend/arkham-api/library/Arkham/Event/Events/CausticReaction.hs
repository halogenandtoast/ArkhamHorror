module Arkham.Event.Events.CausticReaction (causticReaction) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Projection ()
import Arkham.Modifier

newtype CausticReaction = CausticReaction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

causticReaction :: EventCard CausticReaction
causticReaction = event CausticReaction Cards.causticReaction

instance RunMessage CausticReaction where
  runMessage msg e@(CausticReaction attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      clues <- iid.clues
      skillTestModifiers
        sid
        attrs
        iid
        [SkillModifier #intellect 1, DamageDealt $ if clues >= 2 then 2 else 1]
      chooseFightEnemyWith #intellect sid iid attrs
      pure e
    _ -> CausticReaction <$> liftRunMessage msg attrs
