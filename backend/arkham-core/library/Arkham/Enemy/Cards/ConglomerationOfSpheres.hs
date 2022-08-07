module Arkham.Enemy.Cards.ConglomerationOfSpheres
  ( conglomerationOfSpheres
  , ConglomerationOfSpheres(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype ConglomerationOfSpheres = ConglomerationOfSpheres EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

conglomerationOfSpheres :: EnemyCard ConglomerationOfSpheres
conglomerationOfSpheres = enemyWith
  ConglomerationOfSpheres
  Cards.conglomerationOfSpheres
  (1, Static 6, 4)
  (1, 1)
  (preyL .~ Prey (InvestigatorWithLowestSkill SkillWillpower))

instance HasAbilities ConglomerationOfSpheres where
  getAbilities (ConglomerationOfSpheres x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemyAttacked Timing.After You (SourceWithTrait Melee)
      $ EnemyWithId
      $ toId x
    ]

instance RunMessage ConglomerationOfSpheres where
  runMessage msg e@(ConglomerationOfSpheres attrs) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemyAttacked _ attackSource _)] 1 _
      | isSource attrs source
      -> e <$ push (Discard $ sourceToTarget attackSource)
    _ -> ConglomerationOfSpheres <$> runMessage msg attrs
