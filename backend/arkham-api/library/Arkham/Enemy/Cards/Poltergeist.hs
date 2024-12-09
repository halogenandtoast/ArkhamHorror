module Arkham.Enemy.Cards.Poltergeist (poltergeist, Poltergeist (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Trait

newtype Poltergeist = Poltergeist EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poltergeist :: EnemyCard Poltergeist
poltergeist = enemy Poltergeist Cards.poltergeist (3, Static 2, 4) (0, 2)

instance HasAbilities Poltergeist where
  getAbilities (Poltergeist a) =
    withBaseAbilities a [skillTestAbility $ restrictedAbility a 1 OnSameLocation parleyAction_]

instance HasModifiersFor Poltergeist where
  getModifiersFor (Poltergeist a) = do
    modifySelf
      a
      [CannotBeDamagedByPlayerSourcesExcept $ SourceMatchesAny $ map SourceWithTrait [Spell, Relic]]

instance RunMessage Poltergeist where
  runMessage msg e@(Poltergeist attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ Msg.EnemyDamage (toId attrs) $ nonAttack (attrs.ability 1) 1
      pure e
    _ -> Poltergeist <$> runMessage msg attrs
