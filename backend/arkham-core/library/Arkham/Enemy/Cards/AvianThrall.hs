module Arkham.Enemy.Cards.AvianThrall
  ( AvianThrall(..)
  , avianThrall
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Trait

newtype AvianThrall = AvianThrall EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

avianThrall :: EnemyCard AvianThrall
avianThrall = enemyWith
  AvianThrall
  Cards.avianThrall
  (5, Static 4, 3)
  (1, 1)
  (preyL .~ Prey (InvestigatorWithLowestSkill SkillIntellect))

instance HasSet Trait env AssetId => HasModifiersFor env AvianThrall where
  getModifiersFor (AssetSource aid) target (AvianThrall attrs)
    | isTarget attrs target = do
      traits <- getSet aid
      pure $ toModifiers
        attrs
        [ EnemyFight (-3) | any (`elem` [Ranged, Firearm, Spell]) traits ]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env AvianThrall where
  runMessage msg (AvianThrall attrs) = AvianThrall <$> runMessage msg attrs
