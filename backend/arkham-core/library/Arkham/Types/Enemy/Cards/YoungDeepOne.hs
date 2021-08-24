module Arkham.Types.Enemy.Cards.YoungDeepOne
  ( YoungDeepOne(..)
  , youngDeepOne
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType
import qualified Arkham.Types.Timing as Timing

newtype YoungDeepOne = YoungDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youngDeepOne :: EnemyCard YoungDeepOne
youngDeepOne = enemyWith
  YoungDeepOne
  Cards.youngDeepOne
  (3, Static 3, 3)
  (1, 1)
  (preyL .~ LowestSkill SkillCombat)

instance HasAbilities env YoungDeepOne where
  getAbilities iid window (YoungDeepOne a) =
    withBaseAbilities iid window a $ pure
      [ mkAbility a 1
        $ ForcedAbility
        $ EnemyEngaged Timing.After You
        $ EnemyWithId
        $ toId a
      ]

instance EnemyRunner env => RunMessage env YoungDeepOne where
  runMessage msg e@(YoungDeepOne attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      e <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> YoungDeepOne <$> runMessage msg attrs
