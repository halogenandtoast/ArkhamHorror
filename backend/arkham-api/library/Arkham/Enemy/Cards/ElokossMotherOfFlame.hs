module Arkham.Enemy.Cards.ElokossMotherOfFlame (elokossMotherOfFlame) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers hiding (roundModifier)
import Arkham.Matcher

newtype ElokossMotherOfFlame = ElokossMotherOfFlame EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor ElokossMotherOfFlame where
  getModifiersFor (ElokossMotherOfFlame a) = do
    healthModifier <- perPlayer 5
    modifySelf a [HealthModifier healthModifier]

elokossMotherOfFlame :: EnemyCard ElokossMotherOfFlame
elokossMotherOfFlame = enemy ElokossMotherOfFlame Cards.elokossMotherOfFlame (5, Static 5, 5) (3, 3)

instance HasAbilities ElokossMotherOfFlame where
  getAbilities (ElokossMotherOfFlame a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEvaded #after You (be a)

instance RunMessage ElokossMotherOfFlame where
  runMessage msg e@(ElokossMotherOfFlame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) iid (CannotBeAttackedBy (be attrs))
      readyThis attrs
      pure e
    _ -> ElokossMotherOfFlame <$> liftRunMessage msg attrs
