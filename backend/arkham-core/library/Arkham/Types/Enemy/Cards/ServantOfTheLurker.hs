module Arkham.Types.Enemy.Cards.ServantOfTheLurker
  ( servantOfTheLurker
  , ServantOfTheLurker(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyAttacks)
import Arkham.Types.Prey
import Arkham.Types.SkillType
import Arkham.Types.Timing qualified as Timing

newtype ServantOfTheLurker = ServantOfTheLurker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfTheLurker :: EnemyCard ServantOfTheLurker
servantOfTheLurker = enemyWith
  ServantOfTheLurker
  Cards.servantOfTheLurker
  (4, Static 5, 2)
  (2, 2)
  (preyL .~ LowestSkill SkillAgility)

instance HasAbilities ServantOfTheLurker where
  getAbilities (ServantOfTheLurker x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemyAttacks Timing.When You
      $ EnemyWithId
      $ toId x
    ]

instance EnemyRunner env => RunMessage env ServantOfTheLurker where
  runMessage msg e@(ServantOfTheLurker attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (DiscardTopOfDeck iid 2 Nothing)
    _ -> ServantOfTheLurker <$> runMessage msg attrs
