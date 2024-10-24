module Arkham.Event.Events.BlindingLight2 (blindingLight2, BlindingLight2 (..)) where

import Arkham.Aspect hiding (aspect)
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTestEnemyTarget)
import Arkham.Matcher

newtype BlindingLight2 = BlindingLight2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2 :: EventCard BlindingLight2
blindingLight2 = event BlindingLight2 Cards.blindingLight2

instance RunMessage BlindingLight2 where
  runMessage msg e@(BlindingLight2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]) attrs iid do
        push $ LoseActions iid (toSource attrs) 1
        assignHorror iid attrs 1
      aspect iid attrs (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid attrs)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      withSkillTestEnemyTarget (nonAttackEnemyDamage iid 2)
      pure e
    _ -> BlindingLight2 <$> liftRunMessage msg attrs
