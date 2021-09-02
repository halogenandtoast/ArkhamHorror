module Arkham.Types.Enemy.Cards.ConglomerationOfSpheres
  ( conglomerationOfSpheres
  , ConglomerationOfSpheres(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Prey
import Arkham.Types.SkillType
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype ConglomerationOfSpheres = ConglomerationOfSpheres EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conglomerationOfSpheres :: EnemyCard ConglomerationOfSpheres
conglomerationOfSpheres = enemyWith
  ConglomerationOfSpheres
  Cards.conglomerationOfSpheres
  (1, Static 6, 4)
  (1, 1)
  (preyL .~ LowestSkill SkillWillpower)

instance HasAbilities ConglomerationOfSpheres where
  getAbilities (ConglomerationOfSpheres x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemyAttacked Timing.After You (SourceWithTrait Melee)
      $ EnemyWithId
      $ toId x
    ]

instance EnemyRunner env => RunMessage env ConglomerationOfSpheres where
  runMessage msg e@(ConglomerationOfSpheres attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemyAttacked _ attackSource _)] 1 _
      | isSource attrs source
      -> e <$ push (Discard $ sourceToTarget attackSource)
    _ -> ConglomerationOfSpheres <$> runMessage msg attrs
