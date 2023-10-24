module Arkham.Enemy.Cards.OtherwordlyMeddler (
  otherwordlyMeddler,
  OtherwordlyMeddler (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype OtherwordlyMeddler = OtherwordlyMeddler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherwordlyMeddler :: EnemyCard OtherwordlyMeddler
otherwordlyMeddler = enemy OtherwordlyMeddler Cards.otherwordlyMeddler (4, PerPlayer 5, 3) (2, 2)

instance HasAbilities OtherwordlyMeddler where
  getAbilities (OtherwordlyMeddler attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (exists $ EnemyWithId (toId attrs) <> EnemyWithAnyDoom)
          $ ForcedAbility
          $ EnemyTakeDamage #when AttackDamageEffect (EnemyWithId $ toId attrs) AnySource
      , mkAbility attrs 2 $ ForcedAbility $ InvestigatorDefeated #after ByAny Anyone
      ]

instance RunMessage OtherwordlyMeddler where
  runMessage msg e@(OtherwordlyMeddler attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ PlaceDoom (toAbilitySource attrs 2) (toTarget attrs) 3
      pure e
    _ -> OtherwordlyMeddler <$> runMessage msg attrs
