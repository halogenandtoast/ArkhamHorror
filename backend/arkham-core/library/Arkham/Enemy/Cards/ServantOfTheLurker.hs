module Arkham.Enemy.Cards.ServantOfTheLurker (
  servantOfTheLurker,
  ServantOfTheLurker (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype ServantOfTheLurker = ServantOfTheLurker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

servantOfTheLurker :: EnemyCard ServantOfTheLurker
servantOfTheLurker =
  enemyWith
    ServantOfTheLurker
    Cards.servantOfTheLurker
    (4, Static 5, 2)
    (2, 2)
    (preyL .~ Prey (InvestigatorWithLowestSkill SkillAgility))

instance HasAbilities ServantOfTheLurker where
  getAbilities (ServantOfTheLurker x) =
    withBaseAbilities
      x
      [ mkAbility x 1
          $ ForcedAbility
          $ EnemyAttacks Timing.When You AnyEnemyAttack
          $ EnemyWithId
          $ toId x
      ]

instance RunMessage ServantOfTheLurker where
  runMessage msg e@(ServantOfTheLurker attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          e <$ push (DiscardTopOfDeck iid 2 (toAbilitySource attrs 1) Nothing)
    _ -> ServantOfTheLurker <$> runMessage msg attrs
