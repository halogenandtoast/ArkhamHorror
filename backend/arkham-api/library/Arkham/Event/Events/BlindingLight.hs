module Arkham.Event.Events.BlindingLight where

import Arkham.Aspect hiding (aspect)
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTestEnemyTarget)
import Arkham.Matcher

newtype BlindingLight = BlindingLight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight :: EventCard BlindingLight
blindingLight = event BlindingLight Cards.blindingLight

instance RunMessage BlindingLight where
  runMessage msg e@(BlindingLight attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]) attrs iid do
        push $ LoseActions iid (toSource attrs) 1
      aspect iid attrs (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid attrs)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTestEnemyTarget (nonAttackEnemyDamage iid 1)
      pure e
    _ -> BlindingLight <$> liftRunMessage msg attrs
