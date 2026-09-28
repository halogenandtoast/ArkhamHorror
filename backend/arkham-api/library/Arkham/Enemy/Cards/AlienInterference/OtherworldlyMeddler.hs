module Arkham.Enemy.Cards.AlienInterference.OtherworldlyMeddler (
  otherworldlyMeddler,
  OtherworldlyMeddler (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.CardDefs.AlienInterference qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Damage (reduceDamageDealt)

newtype OtherworldlyMeddler = OtherworldlyMeddler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyMeddler :: EnemyCard OtherworldlyMeddler
otherworldlyMeddler = enemy OtherworldlyMeddler Cards.otherworldlyMeddler

instance HasAbilities OtherworldlyMeddler where
  getAbilities (OtherworldlyMeddler attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (exists $ EnemyWithId (toId attrs) <> EnemyWithAnyDoom)
          $ ForcedAbility
          $ EnemyTakeDamage #when Matcher.AttackDamageEffect (EnemyWithId $ toId attrs) AnyValue AnySource
      , mkAbility attrs 2 $ ForcedAbility $ InvestigatorDefeated #after ByAny Anyone
      ]

instance RunMessage OtherworldlyMeddler where
  runMessage msg e@(OtherworldlyMeddler attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      reduceDamageDealt attrs.id 1
      push $ RemoveDoom (toAbilitySource attrs 1) (toTarget attrs) 1
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ PlaceDoom (toAbilitySource attrs 2) (toTarget attrs) 3
      pure e
    _ -> OtherworldlyMeddler <$> liftRunMessage msg attrs
