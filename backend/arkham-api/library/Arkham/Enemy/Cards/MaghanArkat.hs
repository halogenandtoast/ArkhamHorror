module Arkham.Enemy.Cards.MaghanArkat (maghanArkat) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (Mutated))

newtype MaghanArkat = MaghanArkat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maghanArkat :: EnemyCard MaghanArkat
maghanArkat = enemy MaghanArkat Cards.maghanArkat

instance HasAbilities MaghanArkat where
  getAbilities (MaghanArkat a) =
    extend1 a $ mkAbility a 1 $ forced $ PhaseEnds #when #mythos

instance RunMessage MaghanArkat where
  runMessage msg e@(MaghanArkat attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (EnemyWithTrait Mutated <> EnemyAt (locationWithEnemy attrs)) \mutated ->
        placeMutations (attrs.ability 1) mutated 1
      doStep 1 msg
      pure e
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      mutations <- getMutations attrs.id
      when (mutations > 0) do
        selectEach (investigatorAt (locationWithEnemy attrs)) \iid ->
          assignDamage iid (attrs.ability 1) mutations
      pure e
    _ -> MaghanArkat <$> liftRunMessage msg attrs
