module Arkham.Location.Cards.EasternRidge (easternRidge) where

import Arkham.Ability
import Arkham.Helpers.Window (attackedEnemy, windowSkillTestId)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype EasternRidge = EasternRidge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easternRidge :: LocationCard EasternRidge
easternRidge = location EasternRidge Cards.easternRidge 0 (Static 0)

instance HasAbilities EasternRidge where
  getAbilities (EasternRidge a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ mkAbility a 1
      $ triggered (AttemptToFight #when You (enemyAt a)) (ResourceCost 3)

instance RunMessage EasternRidge where
  runMessage msg l@(EasternRidge attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (attackedEnemy &&& windowSkillTestId -> (enemy, sid)) _ -> do
      skillTestModifier sid (attrs.ability 1) enemy (EnemyFight (-2))
      pure l
    _ -> EasternRidge <$> liftRunMessage msg attrs
