module Arkham.Event.Events.CausticReaction2 (causticReaction2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Projection ()
import Arkham.Modifier

newtype CausticReaction2 = CausticReaction2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

causticReaction2 :: EventCard CausticReaction2
causticReaction2 = event CausticReaction2 Cards.causticReaction2

instance RunMessage CausticReaction2 where
  runMessage msg e@(CausticReaction2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      clues <- iid.clues
      let mods = SkillModifier #intellect 1 : [DamageDealt 1 | clues >= 2]
      skillTestModifiers sid attrs iid mods
      chooseFightEnemyWith #intellect sid iid attrs
      pure e
    _ -> CausticReaction2 <$> liftRunMessage msg attrs
